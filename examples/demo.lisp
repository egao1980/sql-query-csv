;;;; sql-query-csv demo — run from repo root:
;;;;   ros -l examples/demo.lisp -q
;;;;
;;;; Requires sibling sql-query on CL_SOURCE_REGISTRY (workspace default).

(require :asdf)
(asdf:load-system "sql-query-csv")

(defpackage #:sql-query-csv/demo
  (:use #:cl #:sql-query #:sql-query-csv)
  (:shadowing-import-from #:sql-query #:count #:union))

(in-package #:sql-query-csv/demo)

(defun demo-path ()
  (merge-pathnames "examples/users.csv"
                   (asdf:system-source-directory "sql-query-csv")))

(defun run ()
  (let* ((d (csv-catalog :users (demo-path)))
         (stmt (select
                (columns :id :name (label :score :pts))
                (from :users)
                (where (sql-and (:= :active 1)
                                (sql-or (sql-like :city "London")
                                        (:> :score 90))
                                (sql-between :score 70 100)))
                (order-by '(:score :desc))
                (limit 3)))
         (form (compile-query-form stmt :dialect d))
         (fn (compile-query stmt :dialect d))
         (rows (funcall fn)))
    (format t "~&;; --- compile-query-form (emitted Lisp) ---~%~S~%~%" form)
    (format t ";; --- query-csv results (top active London-or-high-score) ---~%")
    (dolist (r rows)
      (format t "  ~{~s ~s~^  ~}~%" r))
    (format t "~&;; --- same via query-csv ---~%~S~%"
            (query-csv stmt :dialect d))
    rows))

(run)
(uiop:quit 0)
