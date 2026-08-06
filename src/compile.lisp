(in-package #:sql-query-csv)

;;; ---------------------------------------------------------------------------
;;; SELECT AST → Lisp forms / closures (wave-1: single table)
;;; ---------------------------------------------------------------------------

(defun %clause (stmt type)
  (find-if (lambda (c) (typep c type)) (sql-query::statement-clauses stmt)))

(defun %unsupported (dialect feature message)
  (error 'sql-dialect-unsupported
         :feature feature
         :dialect dialect
         :message message))

(defun %ensure-select (statement dialect)
  (unless (typep statement 'select-statement)
    (error 'sql-query-error
           :message "csv compile-query supports select-statement only"))
  (when (%clause statement 'sql-query::join-clause)
    (%unsupported dialect :join "csv wave-1: no JOINs"))
  (when (%clause statement 'sql-query::group-by-clause)
    (%unsupported dialect :group-by "csv wave-1: no GROUP BY"))
  (when (%clause statement 'sql-query::having-clause)
    (%unsupported dialect :having "csv wave-1: no HAVING"))
  (when (%clause statement 'sql-query::with-cte-clause)
    (%unsupported dialect :cte "csv wave-1: no CTE"))
  (when (%clause statement 'sql-query::distinct-clause)
    (%unsupported dialect :distinct "csv wave-1: no DISTINCT"))
  (unless (%clause statement 'sql-query::from-clause)
    (error 'sql-query-error :message "csv: SELECT requires FROM")))

(defun %cmp (op a b)
  "Compare with light numeric coercion when both sides look numeric."
  (flet ((num (x)
           (ctypecase x
             (null nil)
             (number x)
             (string
              (or (ignore-errors (parse-integer x :junk-allowed nil))
                  (ignore-errors
                    (let (*read-eval*)
                      (let ((v (read-from-string x)))
                        (and (numberp v) v))))
                  x))
             (t x))))
    (let ((aa (num a))
          (bb (num b)))
      (cond
        ((and (numberp aa) (numberp bb))
         (funcall op aa bb))
        ((or (eq op #'=) (eq op #'equal))
         (equal a b))
        ((eq op #'/=)
         (not (equal a b)))
        (t
         (funcall op (princ-to-string a) (princ-to-string b)))))))

(defun emit-lisp-expr (dialect node)
  "Return a Lisp form evaluating NODE with free variable ROW."
  (declare (ignorable dialect))
  (cond
    ((typep node 'column-ref)
     `(csv-col row
               ',(intern (string-upcase (ident-string (column-ref-name node)))
                         :keyword)))
    ((typep node 'sql-query::literal)
     `',(sql-query::literal-value node))
    ((typep node 'typed-value)
     `',(sql-query::typed-value-value node))
    ((typep node 'bind-param)
     `',(bind-param-effective-value node))
    ((typep node 'sql-query::labeled-expr)
     (emit-lisp-expr dialect (sql-query::labeled-expr-of node)))
    ((typep node 'sql-query::unary-op)
     (let ((op (sql-query::unary-op-op node))
           (arg (emit-lisp-expr dialect (sql-query::unary-op-operand node))))
       (ecase op
         (:not `(not ,arg))
         (:- `(- ,arg)))))
    ((typep node 'binary-op)
     (let ((op (binary-op-op node))
           (l (emit-lisp-expr dialect (binary-op-left node)))
           (r (emit-lisp-expr dialect (binary-op-right node))))
       (case op
         (:= `(%cmp #'= ,l ,r))
         (:!= `(%cmp #'/= ,l ,r))
         (:< `(%cmp #'< ,l ,r))
         (:> `(%cmp #'> ,l ,r))
         (:<= `(%cmp #'<= ,l ,r))
         (:>= `(%cmp #'>= ,l ,r))
         (:+ `(+ ,l ,r))
         (:- `(- ,l ,r))
         (:* `(* ,l ,r))
         (:/ `(/ ,l ,r))
         (otherwise
          (error 'sql-query-error
                 :message (format nil "csv: unsupported binary op ~s" op))))))
    ((typep node 'sql-query::nary-op)
     (let ((op (sql-query::nary-op-op node))
           (args (mapcar (lambda (a) (emit-lisp-expr dialect a))
                         (sql-query::nary-op-operands node))))
       (ecase op
         (:and `(and ,@args))
         (:or `(or ,@args)))))
    ((typep node 'sql-query::like-op)
     (let ((l (emit-lisp-expr dialect (sql-query::like-op-left node)))
           (p (emit-lisp-expr dialect (sql-query::like-op-pattern node))))
       (if (sql-query::like-op-not-p node)
           `(not (like-match (princ-to-string ,l) (princ-to-string ,p)))
           `(like-match (princ-to-string ,l) (princ-to-string ,p)))))
    ((typep node 'sql-query::is-null-op)
     (let ((a (emit-lisp-expr dialect (sql-query::is-null-op-operand node))))
       (if (sql-query::is-null-op-not-p node)
           `(not (null ,a))
           `(null ,a))))
    ((typep node 'sql-query::in-op)
     (let ((l (emit-lisp-expr dialect (sql-query::in-op-left node)))
           (vals (mapcar (lambda (v) (emit-lisp-expr dialect v))
                         (sql-query::in-op-values node))))
       (if (sql-query::in-op-not-p node)
           `(not (member ,l (list ,@vals) :test #'equal))
           `(member ,l (list ,@vals) :test #'equal))))
    ((typep node 'sql-query::between-op)
     (let ((a (emit-lisp-expr dialect (sql-query::between-operand node)))
           (lo (emit-lisp-expr dialect (sql-query::between-low node)))
           (hi (emit-lisp-expr dialect (sql-query::between-high node))))
       (if (sql-query::between-not-p node)
           `(not (and (%cmp #'>= ,a ,lo) (%cmp #'<= ,a ,hi)))
           `(and (%cmp #'>= ,a ,lo) (%cmp #'<= ,a ,hi)))))
    (t
     (error 'sql-query-error
            :message (format nil "csv: cannot emit lisp for ~s" (class-of node))))))

(defun %project-key (item)
  (cond
    ((typep item 'sql-query::labeled-expr)
     (intern (string-upcase (ident-string (sql-query::labeled-name item)))
             :keyword))
    ((typep item 'column-ref)
     (intern (string-upcase (ident-string (column-ref-name item))) :keyword))
    (t :expr)))

(defun %project-pairs (dialect items)
  (loop for item in items
        for key = (%project-key item)
        for src = (if (typep item 'sql-query::labeled-expr)
                      (sql-query::labeled-expr-of item)
                      item)
        collect `',key
        collect (emit-lisp-expr dialect src)))

(defun emit-project-form (dialect items)
  (if (null items)
      `(lambda (row) row)
      `(lambda (row) (list ,@(%project-pairs dialect items)))))

(defun emit-pred-form (dialect where-clause)
  (if where-clause
      `(lambda (row)
         ,(emit-lisp-expr dialect (sql-query::where-expr where-clause)))
      `(lambda (row)
         (declare (ignore row))
         t)))

(defun %order-steps (dialect specs)
  (loop for (expr dir) in specs
        for form = (emit-lisp-expr dialect expr)
        collect `(let ((va (let ((row a)) ,form))
                       (vb (let ((row b)) ,form)))
                   (cond ((%cmp #'< va vb)
                          (return-from order ,(eq dir :asc)))
                         ((%cmp #'> va vb)
                          (return-from order ,(eq dir :desc)))))))

(defun emit-order-form (dialect order-clause)
  (when order-clause
    (let ((specs (sql-query::order-by-items order-clause)))
      `(lambda (a b)
         (block order
           ,@(%order-steps dialect specs)
           nil)))))

(defun compile-query-form (statement &key (dialect nil dialect-p) backend)
  "Return a Lisp form `(lambda () rows)` for STATEMENT on a csv-dialect."
  (let ((d (cond (backend backend)
                 (dialect-p dialect)
                 (t (or *sql-dialect*
                        (gethash :csv *sql-dialect-registry*)
                        (error 'sql-query-error
                               :message "csv: no dialect/backend"))))))
    (check-type d csv-dialect)
    (%ensure-select statement d)
    (let* ((from (%clause statement 'sql-query::from-clause))
           (table-name (sql-query::from-table from))
           (cols (%clause statement 'sql-query::columns-clause))
           (items (and cols (sql-query::columns-items cols)))
           (where (%clause statement 'sql-query::where-clause))
           (order (%clause statement 'sql-query::order-by-clause))
           (lim (%clause statement 'sql-query::limit-clause))
           (off (%clause statement 'sql-query::offset-clause))
           (limit-n (and lim (limit-count lim)))
           (offset-n (and off (offset-count off)))
           (table-key (%table-key table-name))
           (err-msg (format nil "csv: unknown table ~s" table-name))
           (order-form (emit-order-form d order)))
      `(lambda ()
         (let* ((table (or (gethash ,table-key (csv-dialect-tables ,d))
                           (error 'sql-query-error :message ,err-msg)))
                (rows (copy-list (load-csv-table table)))
                (pred ,(emit-pred-form d where))
                (proj ,(emit-project-form d items))
                ,@(when order-form `((ord ,order-form))))
           (setf rows (remove-if-not pred rows))
           ,@(when order-form
               `((setf rows (stable-sort rows ord))))
           ,@(when offset-n
               `((setf rows (nthcdr ,offset-n rows))))
           ,@(when limit-n
               `((setf rows (subseq rows 0 (min ,limit-n (length rows))))))
           (mapcar proj rows))))))

(defun compile-query (statement &key dialect backend)
  "Compile STATEMENT to a zero-arg function returning plist rows."
  (compile nil (compile-query-form statement :dialect dialect :backend backend)))

(defun query-csv (statement &key dialect backend)
  "Compile and immediately run STATEMENT; return plist rows."
  (funcall (compile-query statement :dialect dialect :backend backend)))
