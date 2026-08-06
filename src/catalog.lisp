(in-package #:sql-query-csv)

(defclass csv-table ()
  ((name :initarg :name :reader csv-table-name)
   (source :initarg :source :reader csv-table-source
           :documentation "Pathname designator or list of plist rows.")
   (header :initarg :header :reader csv-table-header :initform t)
   (coerce-numbers :initarg :coerce-numbers :reader csv-table-coerce-numbers
                   :initform t)
   (cache :initform nil :accessor csv-table-cache)))

(defclass csv-dialect (sql-dialect)
  ((tables :initform (make-hash-table :test #'equal)
           :accessor csv-dialect-tables
           :documentation "Map of downcased table name string → csv-table.")
   (coerce-numbers :initarg :coerce-numbers :initform t
                   :accessor csv-dialect-coerce-numbers))
  (:documentation "sql-query target that compiles SELECT to Lisp over CSV sources."))

(defun make-csv-dialect (&key (coerce-numbers t) catalog)
  (let ((d (make-instance 'csv-dialect :coerce-numbers coerce-numbers)))
    (when catalog
      ;; Alternating NAME source [NAME source …] — no per-table keyword options here.
      (loop for (name source) on catalog by #'cddr
            do (register-csv-table d name source)))
    d))

(defun csv-catalog (&rest plist)
  "Build a csv-dialect from alternating NAME source [&key header coerce-numbers].
   Example: (csv-catalog :users #p\"users.csv\" :orders rows-list)"
  (make-csv-dialect :catalog plist))

(defun %table-key (name)
  (string-downcase (ident-string name)))

(defun register-csv-table (dialect name source &key (header t) (coerce-numbers nil coerce-p))
  (let* ((coer (if coerce-p coerce-numbers (csv-dialect-coerce-numbers dialect)))
         (table (make-instance 'csv-table
                               :name name
                               :source source
                               :header header
                               :coerce-numbers coer)))
    (setf (gethash (%table-key name) (csv-dialect-tables dialect)) table)
    table))

(defun find-csv-table (dialect name)
  (or (gethash (%table-key name) (csv-dialect-tables dialect))
      (error 'sql-query-error
             :message (format nil "csv: unknown table ~s" name))))

(defun load-csv-table (table)
  (or (csv-table-cache table)
      (let ((rows
              (let ((src (csv-table-source table)))
                (ctypecase src
                  (list src)
                  ((or string pathname)
                   (read-csv-file src
                                  :header (csv-table-header table)
                                  :coerce-numbers (csv-table-coerce-numbers table)))))))
        (setf (csv-table-cache table) rows)
        rows)))

(defun use-csv-dialect (&rest catalog-plist)
  "Register :csv dialect (optionally with CATALOG plist) and set *SQL-DIALECT*."
  (let ((d (apply #'csv-catalog catalog-plist)))
    (setf *sql-dialect* d)
    (register-sql-dialect :csv d)
    d))
