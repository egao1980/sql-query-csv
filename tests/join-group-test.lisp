(in-package #:sql-query-csv/tests)

(defun %orders-rows ()
  '((:id 100 :uid 1 :item "book" :qty 2)
    (:id 101 :uid 1 :item "pen" :qty 5)
    (:id 102 :uid 2 :item "lamp" :qty 1)
    (:id 103 :uid 4 :item "desk" :qty 1)
    (:id 104 :uid 9 :item "ghost" :qty 1)))

(defun %shop ()
  (csv-catalog :users (%users-rows)
               :orders (%orders-rows)))

(deftest distinct-cities
  (let* ((d (%dialect))
         (rows (query-csv
                (select (distinct) (columns :city)
                        (from :users)
                        (where (:= :active 1))
                        (order-by :city))
                :dialect d)))
    (ok (equal '("Cambridge" "London" "New York")
               (mapcar (lambda (r) (getf r :city)) rows)))))

(deftest group-by-count-sum
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns :city
                                 (label (count :*) :n)
                                 (label (sql-func :sum :score) :total))
                        (from :users)
                        (where (:= :active 1))
                        (group-by :city)
                        (order-by :city))
                :dialect d)))
    (ok (equal 3 (length rows)))
    (let ((london (find "London" rows :key (lambda (r) (getf r :city)) :test #'string=)))
      (ok (equal 2 (getf london :n)))
      (ok (equal 178 (getf london :total))))))

(deftest group-by-having
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns :city (label (count :*) :n))
                        (from :users)
                        (group-by :city)
                        (having (:> (count :*) 1)))
                :dialect d)))
    (ok (equal '("London")
               (mapcar (lambda (r) (getf r :city)) rows)))
    (ok (equal 2 (getf (first rows) :n)))))

(deftest scalar-count
  (let* ((d (%dialect))
         (rows (query-csv
                (select (columns (label (count :*) :n))
                        (from :users)
                        (where (:= :active 1)))
                :dialect d)))
    (ok (equal 1 (length rows)))
    (ok (equal 4 (getf (first rows) :n)))))

(deftest inner-join
  (let* ((d (%shop))
         (rows (query-csv
                (select (columns :users.name :orders.item :orders.qty)
                        (from :users)
                        (join :orders (on (:= :users.id :orders.uid)))
                        (order-by :orders.id))
                :dialect d)))
    (ok (equal 4 (length rows)) "uid=9 has no user; alan inactive still joins")
    (ok (equal "ada" (getf (first rows) :name)))
    (ok (equal "book" (getf (first rows) :item)))
    (ok (equal 2 (getf (first rows) :qty)))))

(deftest left-join
  (let* ((d (%shop))
         (rows (query-csv
                (select (columns :users.name :orders.item)
                        (from :users)
                        (left-join :orders (on (:= :users.id :orders.uid)))
                        (where (:= :users.active 1))
                        (order-by :users.id :orders.id))
                :dialect d)))
    ;; active users: ada(2 orders), grace(1), barbara(1), donald(0 → null item)
    (ok (equal 5 (length rows)))
    (let ((donald (find "donald" rows :key (lambda (r) (getf r :name)) :test #'string=)))
      (ok donald)
      (ok (null (getf donald :item))))))

(deftest join-then-group
  (let* ((d (%shop))
         (rows (query-csv
                (select (columns :users.name (label (count :*) :orders)
                                 (label (sql-func :sum :orders.qty) :units))
                        (from :users)
                        (join :orders (on (:= :users.id :orders.uid)))
                        (group-by :users.name)
                        (order-by :users.name))
                :dialect d)))
    (let ((ada (find "ada" rows :key (lambda (r) (getf r :name)) :test #'string=)))
      (ok (equal 2 (getf ada :orders)))
      (ok (equal 7 (getf ada :units))))))

(deftest example-files-join
  (let* ((d (csv-catalog :users (%example-csv)
                         :orders (merge-pathnames "examples/orders.csv"
                                                  (asdf:system-source-directory "sql-query-csv"))))
         (rows (query-csv
                (select (columns :users.name :orders.item)
                        (from :users)
                        (join :orders (on (:= :users.id :orders.uid)))
                        (where (:= :users.name "ada"))
                        (order-by :orders.item))
                :dialect d)))
    (ok (equal '("book" "pen")
               (mapcar (lambda (r) (getf r :item)) rows)))))

(deftest full-join-unsupported
  (ok (signals (query-csv
                (select (columns :id)
                        (from :users)
                        (full-join :orders (on (:= :users.id :orders.uid))))
                :dialect (%shop))
               'sql-dialect-unsupported)))

(deftest distinct-on-unsupported
  (ok (signals (query-csv
                (select (distinct :city) (columns :city :name)
                        (from :users))
                :dialect (%dialect))
               'sql-dialect-unsupported)))
