(defsystem "sql-query-csv"
  :version "0.1.0"
  :description "sql-query backend — compile SELECT AST to Lisp over CSV files"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-query" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "csv")
               (:file "catalog")
               (:file "compile"))
  :in-order-to ((test-op (test-op "sql-query-csv/tests"))))

(defsystem "sql-query-csv/tests"
  :depends-on ("sql-query-csv" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "csv-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
