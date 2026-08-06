(in-package #:sql-query-csv)

;;; Runtime helpers used by compiled query lambdas.

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
             (t x)))
         (str (x) (princ-to-string (if (null x) "" x))))
    (let ((aa (num a))
          (bb (num b)))
      (cond
        ((and (numberp aa) (numberp bb))
         (funcall op aa bb))
        ((or (eq op #'=) (eq op #'equal))
         (equal a b))
        ((eq op #'/=)
         (not (equal a b)))
        ((eq op #'<) (string< (str a) (str b)))
        ((eq op #'>) (string> (str a) (str b)))
        ((eq op #'<=) (string<= (str a) (str b)))
        ((eq op #'>=) (string>= (str a) (str b)))
        (t (string< (str a) (str b)))))))

(defun %plist-keys (row)
  (loop for (k _) on row by #'cddr collect k))

(defun %qualify-row (row table-key)
  "Duplicate ROW keys as TABLE.COL (keeps bare keys too)."
  (let ((tk (string-upcase (string table-key))))
    (append
     row
     (loop for (k v) on row by #'cddr
           collect (intern (format nil "~a.~a" tk (symbol-name k)) :keyword)
           collect v))))

(defun %null-row (keys)
  (loop for k in keys collect k collect nil))

(defun %join-inner (left right on-fn)
  (loop for l in left
        nconc (loop for r in right
                    for m = (append l r)
                    when (funcall on-fn m)
                      collect m)))

(defun %join-left (left right on-fn right-keys)
  (loop for l in left
        for matches = (loop for r in right
                            for m = (append l r)
                            when (funcall on-fn m)
                              collect m)
        if matches
          append matches
        else
          collect (append l (%null-row right-keys))))

(defun %join-right (left right on-fn left-keys)
  (loop for r in right
        for matches = (loop for l in left
                            for m = (append l r)
                            when (funcall on-fn m)
                              collect m)
        if matches
          append matches
        else
          collect (append (%null-row left-keys) r)))

(defun %join-cross (left right)
  (loop for l in left
        nconc (loop for r in right collect (append l r))))

(defun %apply-join (left right type on-fn left-keys right-keys)
  (ecase type
    ((:inner) (%join-inner left right on-fn))
    ((:left) (%join-left left right on-fn right-keys))
    ((:right) (%join-right left right on-fn left-keys))
    ((:cross) (%join-cross left right))
    ((:full)
     (error 'sql-query-error :message "csv: FULL OUTER JOIN not supported yet"))))

(defun %distinct-rows (rows)
  (remove-duplicates rows :test #'equal))

(defun %group-partition (rows key-fn)
  "Return list of (key . member-rows), stable by first appearance."
  (let ((order nil)
        (table (make-hash-table :test #'equal)))
    (dolist (r rows)
      (let ((k (funcall key-fn r)))
        (unless (gethash k table)
          (push k order)
          (setf (gethash k table) nil))
        (push r (gethash k table))))
    (mapcar (lambda (k)
              (cons k (nreverse (gethash k table))))
            (nreverse order))))

(defun %num (x)
  (ctypecase x
    (null 0)
    (number x)
    (string (or (ignore-errors (parse-integer x :junk-allowed nil))
                (ignore-errors
                  (let (*read-eval*)
                    (let ((v (read-from-string x)))
                      (and (numberp v) v))))
                0))
    (t 0)))

(defun %agg-count (group expr-fn star-p)
  (if star-p
      (length group)
      (loop for r in group
            count (not (null (funcall expr-fn r))))))

(defun %agg-sum (group expr-fn)
  (loop for r in group sum (%num (funcall expr-fn r))))

(defun %agg-avg (group expr-fn)
  (let ((n 0) (s 0))
    (dolist (r group)
      (let ((v (funcall expr-fn r)))
        (unless (null v)
          (incf n)
          (incf s (%num v)))))
    (if (zerop n) nil (/ s n))))

(defun %agg-min (group expr-fn)
  (let ((best nil) (any nil))
    (dolist (r group)
      (let ((v (funcall expr-fn r)))
        (unless (null v)
          (if (not any)
              (setf best v any t)
              (when (%cmp #'< v best)
                (setf best v))))))
    best))

(defun %agg-max (group expr-fn)
  (let ((best nil) (any nil))
    (dolist (r group)
      (let ((v (funcall expr-fn r)))
        (unless (null v)
          (if (not any)
              (setf best v any t)
              (when (%cmp #'> v best)
                (setf best v))))))
    best))
