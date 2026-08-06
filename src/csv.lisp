(in-package #:sql-query-csv)

;;; Minimal RFC4180-ish CSV reader (no cl-csv dep). Header row → keyword keys.

(defun %parse-csv-line (line)
  (let ((out nil)
        (field (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0)
        (n (length line))
        (in-quotes nil))
    (labels ((push-field ()
               (push (copy-seq field) out)
               (setf (fill-pointer field) 0))
             (add (c) (vector-push-extend c field)))
      (loop while (< i n)
            for c = (char line i)
            do (cond
                 ((and in-quotes (char= c #\")
                       (< (1+ i) n) (char= (char line (1+ i)) #\"))
                  (add #\")
                  (incf i 2))
                 ((char= c #\")
                  (setf in-quotes (not in-quotes))
                  (incf i))
                 ((and (not in-quotes) (char= c #\,))
                  (push-field)
                  (incf i))
                 (t
                  (add c)
                  (incf i))))
      (push-field)
      (nreverse out))))

(defun %header-key (name)
  (intern (string-upcase (string-trim '(#\Space #\Tab) name)) :keyword))

(defun %maybe-number (s)
  (cond
    ((or (null s) (string= s "")) s)
    (t
     (let* ((trimmed (string-trim '(#\Space #\Tab) s))
            (int (ignore-errors (parse-integer trimmed :junk-allowed nil)))
            (num (unless int
                   (ignore-errors
                     (let (*read-eval*)
                       (let ((v (read-from-string trimmed)))
                         (and (numberp v) v)))))))
       (or int num trimmed)))))

(defun read-csv-file (path &key (header t) (coerce-numbers t))
  "Return list of plist rows. PATH is a pathname designator."
  (with-open-file (in (uiop:ensure-pathname path) :direction :input
                                                 :if-does-not-exist :error)
    (let* ((lines (loop for line = (read-line in nil nil)
                        while line
                        unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
                          collect line))
           (raw (mapcar #'%parse-csv-line lines)))
      (unless raw
        (return-from read-csv-file nil))
      (if header
          (let ((keys (mapcar #'%header-key (first raw))))
            (mapcar (lambda (cells)
                      (loop for k in keys
                            for v in cells
                            collect k
                            collect (if coerce-numbers (%maybe-number v) v)))
                    (rest raw)))
          (mapcar (lambda (cells)
                    (loop for i from 0
                          for v in cells
                          collect (intern (format nil "COL~d" i) :keyword)
                          collect (if coerce-numbers (%maybe-number v) v)))
                  raw)))))

(defun csv-col (row key)
  "Get column KEY from plist ROW (keyword or symbol/string name)."
  (let ((k (ctypecase key
             (keyword key)
             (symbol (intern (symbol-name key) :keyword))
             (string (%header-key key)))))
    (getf row k)))

(defun like-match (string pattern)
  "SQL LIKE with % and _ (no ESCAPE)."
  (let ((s (coerce (string string) 'list))
        (p (coerce (string pattern) 'list)))
    (labels ((match (s p)
               (cond
                 ((null p) (null s))
                 ((char= (car p) #\%)
                  (loop for rest on s
                        thereis (match rest (cdr p))
                        finally (return (match nil (cdr p)))))
                 ((null s) nil)
                 ((or (char= (car p) #\_) (char= (car p) (car s)))
                  (match (cdr s) (cdr p)))
                 (t nil))))
      (match s p))))
