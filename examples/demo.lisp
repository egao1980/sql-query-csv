;;;; sql-query-csv demo — run from repo root:
;;;;   ros -l examples/demo.lisp -q

(require :asdf)
(asdf:load-system "sql-query-csv")

(defpackage #:sql-query-csv/demo
  (:use #:cl #:sql-query #:sql-query-csv)
  (:shadowing-import-from #:sql-query #:count #:union))

(in-package #:sql-query-csv/demo)

(defun demo-dir ()
  (merge-pathnames "examples/"
                   (asdf:system-source-directory "sql-query-csv")))

(defun run ()
  (let* ((dir (demo-dir))
         (d (csv-catalog :users (merge-pathnames "users.csv" dir)
                         :orders (merge-pathnames "orders.csv" dir))))
    (format t "~&;; === filter / order ===~%")
    (dolist (r (query-csv
                (select (columns :id :name (label :score :pts))
                        (from :users)
                        (where (sql-and (:= :active 1)
                                        (sql-or (sql-like :city "London")
                                                (:> :score 90))))
                        (order-by '(:score :desc))
                        (limit 3))
                :dialect d))
      (format t "  ~{~s ~s~^  ~}~%" r))

    (format t "~&;; === DISTINCT cities (active) ===~%")
    (dolist (r (query-csv
                (select (distinct) (columns :city)
                        (from :users)
                        (where (:= :active 1))
                        (order-by :city))
                :dialect d))
      (format t "  ~s~%" (getf r :city)))

    (format t "~&;; === GROUP BY city ===~%")
    (dolist (r (query-csv
                (select (columns :city
                                 (label (count :*) :n)
                                 (label (sql-func :sum :score) :total))
                        (from :users)
                        (where (:= :active 1))
                        (group-by :city)
                        (order-by :city))
                :dialect d))
      (format t "  ~{~s ~s~^  ~}~%" r))

    (format t "~&;; === INNER JOIN users ⋈ orders ===~%")
    (dolist (r (query-csv
                (select (columns :users.name :orders.item :orders.qty)
                        (from :users)
                        (join :orders (on (:= :users.id :orders.uid)))
                        (order-by :users.name :orders.item))
                :dialect d))
      (format t "  ~{~s ~s~^  ~}~%" r))

    (format t "~&;; === LEFT JOIN + nulls (donald has no orders) ===~%")
    (dolist (r (query-csv
                (select (columns :users.name :orders.item)
                        (from :users)
                        (left-join :orders (on (:= :users.id :orders.uid)))
                        (where (:= :users.name "donald")))
                :dialect d))
      (format t "  ~{~s ~s~^  ~}~%" r))
    t))

(run)
(uiop:quit 0)
