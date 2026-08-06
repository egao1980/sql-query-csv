(in-package #:sql-query-csv/tests)

(defun %users-rows ()
  '((:id 1 :name "ada" :active 1 :score 90)
    (:id 2 :name "grace" :active 1 :score 95)
    (:id 3 :name "alan" :active 0 :score 80)
    (:id 4 :name "barbara" :active 1 :score 88)))

(defun %dialect ()
  (csv-catalog :users (%users-rows)))

(deftest like-match-basics
  (ok (like-match "ada" "a%"))
  (ok (like-match "ada" "%da"))
  (ok (like-match "ada" "a_a"))
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
         (rows (query-csv
                (select (columns :name)
                        (from :users)
                        (where (:= :active 1))
                        (order-by '(:score :desc))
                        (limit 2))
                :dialect d)))
    (ok (equal '("grace" "ada") (mapcar (lambda (r) (getf r :name)) rows)))))

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
  (let ((form (compile-query-form
               (select (columns :id) (from :users) (where (:= :id 1)))
               :dialect (%dialect))))
    (ok (eq 'lambda (first form)))
    (ok (equal '((:id 1)) (funcall (compile nil form))))))

(deftest csv-file-roundtrip
  (uiop:with-temporary-file (:pathname path :type "csv")
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-line "id,name" out)
      (write-line "10,foo" out)
      (write-line "20,bar" out))
    (let* ((d (csv-catalog :t path))
           (rows (query-csv
                  (select (columns :id :name)
                          (from :t)
                          (where (:> :id 15))
                          (order-by :name))
                  :dialect d)))
      (ok (equal '((:id 20 :name "bar")) rows)))))

(deftest join-unsupported
  (ok (signals (query-csv
                (select (columns :id)
                        (from :users)
                        (join :orders (on (:= :users.id :orders.uid))))
                :dialect (%dialect))
               'sql-dialect-unsupported)))
