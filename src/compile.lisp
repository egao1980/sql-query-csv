(in-package #:sql-query-csv)

;;; ---------------------------------------------------------------------------
;;; SELECT AST → Lisp (joins, DISTINCT, GROUP BY / aggregates)
;;; ---------------------------------------------------------------------------

(defun %clause (stmt type)
  (find-if (lambda (c) (typep c type)) (sql-query::statement-clauses stmt)))

(defun %clauses (stmt type)
  (remove-if-not (lambda (c) (typep c type))
                 (sql-query::statement-clauses stmt)))

(defun %unsupported (dialect feature message)
  (error 'sql-dialect-unsupported
         :feature feature
         :dialect dialect
         :message message))

(defun %ensure-select (statement dialect)
  (unless (typep statement 'select-statement)
    (error 'sql-query-error
           :message "csv compile-query supports select-statement only"))
  (when (%clause statement 'sql-query::with-cte-clause)
    (%unsupported dialect :cte "csv: no CTE"))
  (let ((d (%clause statement 'sql-query::distinct-clause)))
    (when (and d (distinct-on d))
      (%unsupported dialect :distinct-on "csv: DISTINCT ON not supported")))
  (dolist (j (%clauses statement 'sql-query::join-clause))
    (when (sql-query::join-natural j)
      (%unsupported dialect :natural-join "csv: NATURAL JOIN not supported"))
    (when (eq (sql-query::join-type j) :full)
      (%unsupported dialect :full-join "csv: FULL OUTER JOIN not supported")))
  (unless (%clause statement 'sql-query::from-clause)
    (error 'sql-query-error :message "csv: SELECT requires FROM")))

(defun %func-key (node)
  (intern (string-upcase (ident-string (function-call-name node))) :keyword))

(defun %star-arg-p (arg)
  (or (eq arg :*)
      (and (typep arg 'sql-query::raw-sql)
           (string= (sql-query::raw-sql-text arg) "*"))))

(defun %aggregate-node-p (node)
  (and (typep node 'function-call)
       (member (%func-key node) '(:count :sum :avg :min :max) :test #'eq)))

(defun %unwrap (item)
  (if (typep item 'sql-query::labeled-expr)
      (sql-query::labeled-expr-of item)
      item))

(defun %contains-aggregate-p (node)
  (labels ((walk (n)
             (cond
               ((%aggregate-node-p n) t)
               ((typep n 'sql-query::labeled-expr)
                (walk (sql-query::labeled-expr-of n)))
               ((typep n 'binary-op)
                (or (walk (binary-op-left n)) (walk (binary-op-right n))))
               ((typep n 'sql-query::unary-op)
                (walk (sql-query::unary-op-operand n)))
               ((typep n 'sql-query::nary-op)
                (some #'walk (sql-query::nary-op-operands n)))
               ((typep n 'function-call)
                (some #'walk (function-call-args n)))
               (t nil))))
    (walk node)))

(defun %select-has-aggregate-p (items)
  (some (lambda (i) (%contains-aggregate-p (%unwrap i))) items))

;;; ---- expression → form ----
;;; ROW bound for row mode; GROUP (list of rows) for aggregate mode.
;;; Non-agg columns in group mode read from (FIRST GROUP).

(defun emit-lisp-expr (dialect node &key group-p)
  (declare (ignorable dialect))
  (cond
    ((typep node 'column-ref)
     (let ((name (intern (string-upcase (ident-string (column-ref-name node)))
                         :keyword))
           (table (column-ref-table node)))
       (if group-p
           `(csv-col (first group) ',name
                     ,(when table
                        `',(intern (string-upcase (ident-string table)) :keyword)))
           `(csv-col row ',name
                     ,(when table
                        `',(intern (string-upcase (ident-string table)) :keyword))))))
    ((typep node 'sql-query::literal)
     `',(sql-query::literal-value node))
    ((typep node 'typed-value)
     `',(sql-query::typed-value-value node))
    ((typep node 'bind-param)
     `',(bind-param-effective-value node))
    ((typep node 'sql-query::labeled-expr)
     (emit-lisp-expr dialect (sql-query::labeled-expr-of node) :group-p group-p))
    ((typep node 'function-call)
     (emit-function dialect node :group-p group-p))
    ((typep node 'sql-query::unary-op)
     (let ((op (sql-query::unary-op-op node))
           (arg (emit-lisp-expr dialect (sql-query::unary-op-operand node)
                                :group-p group-p)))
       (ecase op
         (:not `(not ,arg))
         (:- `(- ,arg)))))
    ((typep node 'binary-op)
     (let ((op (binary-op-op node))
           (l (emit-lisp-expr dialect (binary-op-left node) :group-p group-p))
           (r (emit-lisp-expr dialect (binary-op-right node) :group-p group-p)))
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
           (args (mapcar (lambda (a)
                           (emit-lisp-expr dialect a :group-p group-p))
                         (sql-query::nary-op-operands node))))
       (ecase op
         (:and `(and ,@args))
         (:or `(or ,@args)))))
    ((typep node 'sql-query::like-op)
     (let ((l (emit-lisp-expr dialect (sql-query::like-op-left node) :group-p group-p))
           (p (emit-lisp-expr dialect (sql-query::like-op-pattern node) :group-p group-p)))
       (if (sql-query::like-op-not-p node)
           `(not (like-match (princ-to-string ,l) (princ-to-string ,p)))
           `(like-match (princ-to-string ,l) (princ-to-string ,p)))))
    ((typep node 'sql-query::is-null-op)
     (let ((a (emit-lisp-expr dialect (sql-query::is-null-op-operand node)
                              :group-p group-p)))
       (if (sql-query::is-null-op-not-p node)
           `(not (null ,a))
           `(null ,a))))
    ((typep node 'sql-query::in-op)
     (let ((l (emit-lisp-expr dialect (sql-query::in-op-left node) :group-p group-p))
           (vals (mapcar (lambda (v)
                           (emit-lisp-expr dialect v :group-p group-p))
                         (sql-query::in-op-values node))))
       (if (sql-query::in-op-not-p node)
           `(not (member ,l (list ,@vals) :test #'equal))
           `(member ,l (list ,@vals) :test #'equal))))
    ((typep node 'sql-query::between-op)
     (let ((a (emit-lisp-expr dialect (sql-query::between-operand node) :group-p group-p))
           (lo (emit-lisp-expr dialect (sql-query::between-low node) :group-p group-p))
           (hi (emit-lisp-expr dialect (sql-query::between-high node) :group-p group-p)))
       (if (sql-query::between-not-p node)
           `(not (and (%cmp #'>= ,a ,lo) (%cmp #'<= ,a ,hi)))
           `(and (%cmp #'>= ,a ,lo) (%cmp #'<= ,a ,hi)))))
    (t
     (error 'sql-query-error
            :message (format nil "csv: cannot emit lisp for ~s" (class-of node))))))

(defun emit-function (dialect node &key group-p)
  (let* ((name (%func-key node))
         (args (function-call-args node))
         (arg0 (first args)))
    (unless group-p
      (error 'sql-query-error
             :message (format nil "csv: aggregate ~s requires GROUP BY (or is the only select)" name)))
    (flet ((arg-fn ()
             `(lambda (row)
                ,(emit-lisp-expr dialect arg0 :group-p nil))))
      (case name
        (:count
         (let ((star (or (null args) (%star-arg-p arg0))))
           `(%agg-count group ,(if star `(lambda (row) (declare (ignore row)) nil) (arg-fn)) ,star)))
        (:sum `(%agg-sum group ,(arg-fn)))
        (:avg `(%agg-avg group ,(arg-fn)))
        (:min `(%agg-min group ,(arg-fn)))
        (:max `(%agg-max group ,(arg-fn)))
        (otherwise
         (error 'sql-query-error
                :message (format nil "csv: unsupported function ~s" name)))))))

(defun %project-key (item)
  (cond
    ((typep item 'sql-query::labeled-expr)
     (intern (string-upcase (ident-string (sql-query::labeled-name item)))
             :keyword))
    ((typep item 'column-ref)
     (intern (string-upcase (ident-string (column-ref-name item))) :keyword))
    ((%aggregate-node-p item)
     (%func-key item))
    (t :expr)))

(defun emit-project-form (dialect items &key group-p)
  (if (null items)
      (if group-p
          `(lambda (group) (copy-list (first group)))
          `(lambda (row) row))
      `(lambda (,(if group-p 'group 'row))
         (list
          ,@(loop for item in items
                  for key = (%project-key item)
                  for src = (%unwrap item)
                  collect `',key
                  collect (emit-lisp-expr dialect src :group-p group-p))))))

(defun emit-pred-form (dialect where-clause &key group-p)
  (if where-clause
      `(lambda (,(if group-p 'group 'row))
         ,(emit-lisp-expr dialect
                          (if group-p
                              (sql-query::having-expr where-clause)
                              (sql-query::where-expr where-clause))
                          :group-p group-p))
      `(lambda (,(if group-p 'group 'row))
         (declare (ignore ,(if group-p 'group 'row)))
         t)))

(defun emit-where-form (dialect where-clause)
  (emit-pred-form dialect where-clause :group-p nil))

(defun emit-having-form (dialect having-clause)
  (emit-pred-form dialect having-clause :group-p t))

(defun %order-steps (dialect specs &key group-p)
  (declare (ignore group-p))
  (loop for (expr dir) in specs
        for form = (emit-lisp-expr dialect expr :group-p nil)
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

(defun %relation-key (table &optional alias)
  (%table-key (or alias table)))

(defun emit-on-form (dialect on-expr)
  (if on-expr
      `(lambda (row)
         ,(emit-lisp-expr dialect on-expr :group-p nil))
      `(lambda (row) (declare (ignore row)) t)))

(defun %using->on (left-key right-key using-cols)
  "Build AND of equalities for USING (col …)."
  (let ((eqs (mapcar (lambda (c)
                       (let* ((name (if (typep c 'column-ref)
                                        (column-ref-name c)
                                        c))
                              (n (ident-string name)))
                         (make-instance 'binary-op
                                        :op :=
                                        :left (col n left-key)
                                        :right (col n right-key))))
                     using-cols)))
    (if (= 1 (length eqs))
        (first eqs)
        (make-instance 'sql-query::nary-op :op :and :operands eqs))))

(defun emit-group-key-form (dialect group-clause)
  (let ((items (sql-query::group-by-items group-clause)))
    `(lambda (row)
       (list ,@(mapcar (lambda (i)
                         (emit-lisp-expr dialect i :group-p nil))
                       items)))))

(defun %emit-join-step (dialect from-key j)
  (let* ((jtype (sql-query::join-type j))
         (jtable (sql-query::join-table j))
         (jalias (sql-query::join-alias j))
         (jkey (%relation-key jtable jalias))
         (on-expr (or (sql-query::join-on j)
                      (when (sql-query::join-using j)
                        (%using->on from-key jkey (sql-query::join-using j)))))
         (err (format nil "csv: unknown join table ~s" jtable)))
    `(let* ((right (mapcar (lambda (r) (%qualify-row r ,jkey))
                           (load-csv-table
                            (or (gethash ,jkey (csv-dialect-tables dialect))
                                (gethash ,(%table-key jtable)
                                         (csv-dialect-tables dialect))
                                (error 'sql-query-error :message ,err)))))
            (on-fn ,(emit-on-form dialect on-expr))
            (lk (%plist-keys (or (first rows) nil)))
            (rk (%plist-keys (or (first right) nil))))
       (setf rows (%apply-join rows right ,jtype on-fn lk rk)))))

(defun %emit-group-step (dialect group having items)
  `(let* ((key-fn ,(if group
                       (emit-group-key-form dialect group)
                       '(lambda (row) (declare (ignore row)) :all)))
          (parts (%group-partition rows key-fn))
          (hav ,(emit-having-form dialect having))
          (proj ,(emit-project-form dialect items :group-p t))
          (out nil))
     (dolist (part parts)
       (let ((group (cdr part)))
         (when (funcall hav group)
           (push (funcall proj group) out))))
     (setf rows (nreverse out))))

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
           (from-table (sql-query::from-table from))
           (from-alias (sql-query::from-alias from))
           (from-key (%relation-key from-table from-alias))
           (joins (%clauses statement 'sql-query::join-clause))
           (cols (%clause statement 'sql-query::columns-clause))
           (items (and cols (sql-query::columns-items cols)))
           (where (%clause statement 'sql-query::where-clause))
           (group (%clause statement 'sql-query::group-by-clause))
           (having (%clause statement 'sql-query::having-clause))
           (distinct-c (%clause statement 'sql-query::distinct-clause))
           (order (%clause statement 'sql-query::order-by-clause))
           (lim (%clause statement 'sql-query::limit-clause))
           (off (%clause statement 'sql-query::offset-clause))
           (limit-n (and lim (limit-count lim)))
           (offset-n (and off (offset-count off)))
           (need-group (or group (%select-has-aggregate-p items)))
           (order-form (emit-order-form d order))
           (err-msg (format nil "csv: unknown table ~s" from-table))
           ;; Pipeline: JOIN → WHERE → (ORDER if ungrouped) → GROUP/PROJECT
           ;; → DISTINCT → (ORDER if grouped) → OFFSET/LIMIT
           ;; Ungrouped ORDER runs before PROJECT so ORDER BY can use non-selected cols.
           (body (append
                  (mapcar (lambda (j) (%emit-join-step d from-key j)) joins)
                  (list `(setf rows (remove-if-not ,(emit-where-form d where) rows)))
                  (when (and order-form (not need-group))
                    (list `(setf rows (stable-sort rows ,order-form))))
                  (list (if need-group
                            (%emit-group-step d group having items)
                            `(setf rows (mapcar ,(emit-project-form d items :group-p nil)
                                                rows))))
                  (when distinct-c
                    (list `(setf rows (%distinct-rows rows))))
                  (when (and order-form need-group)
                    (list `(setf rows (stable-sort rows ,order-form))))
                  (when offset-n
                    (list `(setf rows (nthcdr ,offset-n rows))))
                  (when limit-n
                    (list `(setf rows (subseq rows 0
                                              (min ,limit-n (length rows)))))))))
      (when (and having (not need-group))
        (error 'sql-query-error :message "csv: HAVING without GROUP BY / aggregate"))
      `(lambda ()
         (let* ((dialect ,d)
                (rows (mapcar (lambda (r) (%qualify-row r ,from-key))
                              (load-csv-table
                               (or (gethash ,from-key (csv-dialect-tables dialect))
                                   (gethash ,(%table-key from-table)
                                            (csv-dialect-tables dialect))
                                   (error 'sql-query-error :message ,err-msg))))))
           ,@body
           rows)))))

(defun compile-query (statement &key dialect backend)
  "Compile STATEMENT to a zero-arg function returning plist rows."
  (compile nil (compile-query-form statement :dialect dialect :backend backend)))

(defun query-csv (statement &key dialect backend)
  "Compile and immediately run STATEMENT; return plist rows."
  (funcall (compile-query statement :dialect dialect :backend backend)))
