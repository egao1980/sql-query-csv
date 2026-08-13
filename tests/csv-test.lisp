(in-package #:sql-query-csv/tests)

(defun %users-rows ()
  '((:id 1 :name "ada" :active 1 :score 90 :city "London" :nick "lovelace")
    (:id 2 :name "grace" :active 1 :score 95 :city "New York" :nick "hopper")
    (:id 3 :name "alan" :active 0 :score 80 :city "Manchester" :nick "turing")
    (:id 4 :name "barbara" :active 1 :score 88 :city "London" :nick "liskov")
    (:id 5 :name "donald" :active 1 :score 72 :city "Cambridge" :nick nil)))

(defun %dialect ()
  (csv-catalog :users (%users-rows)))

(defun %example-csv ()
  (merge-pathnames "examples/users.csv"
                   (asdf:system-source-directory "sql-query-csv")))

(deftest like-match-basics
  (ok (like-match "ada" "a%"))
  (ok (like-match "ada" "%da"))
  (ok (like-match "ada" "a_a"))
  (ok (like-match "New York" "%York"))
  (ng (like-match "ada" "b%")))

(deftest select-filter-project
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns :id :name)
                        (from :users)
                        (where (sql-and (:= :active 1)
                                        (:> :score 85))))
                :dialect d)))
    (ok (equal 3 (length rows)))
    (ok (equal '(1 2 4) (mapcar (lambda (r) (getf r :id)) rows)))
    (ok (equal "ada" (getf (first rows) :name)))
    (ng (getf (first rows) :score) "projection drops score")))

(deftest order-limit-offset
  (let* ((d (%dialect))
         (top (query-csv
               (select (columns :name)
                       (from :users)
                       (where (:= :active 1))
                       (order-by '(:score :desc))
                       (limit 2))
               :dialect d))
         (page (query-csv
                (select (columns :name)
                        (from :users)
                        (order-by :id)
                        (limit 2)
                        (offset 2))
                :dialect d)))
    (ok (equal '("grace" "ada") (mapcar (lambda (r) (getf r :name)) top)))
    (ok (equal '("alan" "barbara") (mapcar (lambda (r) (getf r :name)) page)))))

(deftest like-and-between
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns :name)
                        (from :users)
                        (where (sql-and (sql-like :name "%a%")
                                        (sql-between :score 85 92))))
                :dialect d)))
    (ok (equal '("ada" "barbara")
               (sort (mapcar (lambda (r) (getf r :name)) rows) #'string<)))))

(deftest or-in-is-null
  (let* ((d (%dialect))
         (or-rows (query-csv
                   (select (columns :name)
                           (from :users)
                           (where (sql-or (:= :city "Cambridge")
                                          (:= :name "ada"))))
                   :dialect d))
         (in-rows (query-csv
                   (select (columns :id)
                           (from :users)
                           (where (sql-in :id 1 3 5)))
                   :dialect d))
         (null-rows (query-csv
                     (select (columns :name)
                             (from :users)
                             (where (sql-is-null :nick)))
                     :dialect d)))
    (ok (equal '("ada" "donald")
               (sort (mapcar (lambda (r) (getf r :name)) or-rows) #'string<)))
    (ok (equal '(1 3 5) (mapcar (lambda (r) (getf r :id)) in-rows)))
    (ok (equal '("donald") (mapcar (lambda (r) (getf r :name)) null-rows)))))

(deftest arithmetic-in-where
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns :name)
                        (from :users)
                        (where (:> (:+ :score 10) 100)))
                :dialect d)))
    (ok (equal '("grace") (mapcar (lambda (r) (getf r :name)) rows)))))

(deftest bindparam-and-label
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns (label :name :who) :id)
                        (from :users)
                        (where (:= :id (bindparam :id 2))))
                :dialect d)))
    (ok (equal 1 (length rows)))
    (ok (equal "grace" (getf (first rows) :who)))
    (ok (equal 2 (getf (first rows) :id)))))

(deftest compile-query-form-is-lambda
  (let* ((d (%dialect))
         (stmt (select (columns :id) (from :users) (where (:= :id 1))))
         (form (compile-query-form stmt :dialect d))
         (fn (compile-query stmt :dialect d)))
    (ok (eq 'lambda (first form)))
    (ok (equal '((:id 1)) (funcall (compile nil form))))
    (ok (equal '((:id 1)) (funcall fn)) "compiled fn reusable")
    (ok (equal '((:id 1)) (funcall fn)))))

(deftest example-users-csv-file
  "Integration: ship examples/users.csv + same query as demo.lisp."
  (let* ((path (%example-csv))
         (d (csv-catalog :users path))
         (stmt (select
                (columns :id :name (label :score :pts))
                (from :users)
                (where (sql-and (:= :active 1)
                                (sql-or (sql-like :city "London")
                                        (:> :score 90))
                                (sql-between :score 70 100)))
                (order-by '(:score :desc))
                (limit 3)))
         (rows (query-csv stmt :dialect d)))
    (ok (probe-file path) "examples/users.csv present")
    (ok (equal 3 (length rows)))
    ;; grace (95, NY), ada (90, London), barbara (88, London)
    (ok (equal '(2 1 4) (mapcar (lambda (r) (getf r :id)) rows)))
    (ok (equal 95 (getf (first rows) :pts)))
    (let* ((all (load-csv-table (find-csv-table d :users)))
           (grace (find 2 all :key (lambda (r) (getf r :id)))))
      (ok (equal "New York" (getf grace :city))
          "quoted CSV field with space"))))

(deftest csv-file-roundtrip-temp
  (uiop:with-temporary-file (:pathname path :type "csv")
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-line "id,name" out)
      (write-line "10,foo" out)
      (write-line "20,\"bar,baz\"" out))
    (let* ((d (csv-catalog :t path))
           (rows (query-csv
                  (select (columns :id :name)
                          (from :t)
                          (where (:> :id 15))
                          (order-by :name))
                  :dialect d)))
      (ok (equal '((:id 20 :name "bar,baz")) rows)))))

(deftest csv-crlf-line-endings
  "RFC 4180 CRLF records: no #\\Return leaks into keys or values (Windows checkouts)."
  (uiop:with-temporary-file (:pathname path :type "csv")
    (with-open-file (out path :direction :output :if-exists :supersede)
      (dolist (line '("id,name,city" "1,ada,London" "2,grace,\"New York\""))
        (write-string line out)
        (write-char #\Return out)
        (write-char #\Linefeed out)))
    (let ((rows (read-csv-file path)))
      (ok (equal 2 (length rows)))
      (ok (equal '(:id :name :city)
                 (loop for (k nil) on (first rows) by #'cddr collect k))
          "header keys free of #\\Return")
      (ok (equal "London" (getf (first rows) :city)))
      (ok (equal "New York" (getf (second rows) :city))
          "quoted last field free of #\\Return")
      (ok (equal '(1 2) (mapcar (lambda (r) (getf r :id)) rows))
          "numeric coercion unaffected by CRLF"))))

(deftest register-table-and-unknown
  (let ((d (make-csv-dialect)))
    (register-csv-table d :people (%users-rows))
    (ok (equal 1 (length (query-csv
                          (select (columns :id) (from :people) (where (:= :id 1)))
                          :dialect d))))
    (ok (signals (query-csv (select (columns :id) (from :missing))
                            :dialect d)
                 'sql-query-error))))
