# sql-query-csv

Compile [`sql-query`](https://github.com/egao1980/sql-query) **SELECT** ASTs to **Lisp** that runs over CSV tables (in-memory plists or files).

Not a SQL-string dialect — parallel to `compile-sql`:

| API | Target |
|-----|--------|
| `sql-query:compile-sql` | SQL text + params |
| `sql-query-csv:compile-query` | zero-arg function → plist rows |
| `sql-query-csv:compile-query-form` | inspectable `(lambda () …)` form |
| `sql-query-csv:query-csv` | compile + run |

## Wave-1

Single-table `SELECT` · `WHERE` (`= < > AND OR LIKE IN BETWEEN IS NULL` + arith) · `ORDER BY` · `LIMIT`/`OFFSET` · column projection / `label`.

No JOIN / GROUP BY / DISTINCT / CTE / DML.

## Example

Sample data: [`examples/users.csv`](examples/users.csv). Runnable demo:

```bash
# from repo root; sql-query on CL_SOURCE_REGISTRY
ros -l examples/demo.lisp -q
```

```lisp
(asdf:load-system "sql-query-csv")

(use-package :sql-query)
(use-package :sql-query-csv)

(defparameter *d*
  (csv-catalog :users
               (merge-pathnames "examples/users.csv"
                                (asdf:system-source-directory "sql-query-csv"))))

(defparameter *stmt*
  (select
   (columns :id :name (label :score :pts))
   (from :users)
   (where (sql-and (:= :active 1)
                   (sql-or (sql-like :city "London")
                           (:> :score 90))))
   (order-by '(:score :desc))
   (limit 3)))

(compile-query-form *stmt* :dialect *d*)
;; => (LAMBDA ()
;;      (LET* ((TABLE …) (ROWS …) (PRED …) (PROJ …) (ORD …))
;;        …))

(query-csv *stmt* :dialect *d*)
;; => ((:ID 2 :NAME "grace" :PTS 95)
;;     (:ID 1 :NAME "ada" :PTS 90)
;;     (:ID 4 :NAME "barbara" :PTS 88))
```

In-memory catalog (no file):

```lisp
(csv-catalog :users
  '((:id 1 :name "ada" :active 1 :score 90)
    (:id 2 :name "grace" :active 1 :score 95)))
```

## Tests

```bash
ros -e '(asdf:test-system "sql-query-csv")' -q
```

Covers filter/project, ORDER/LIMIT/OFFSET, LIKE/BETWEEN/IN/OR/IS NULL, arith, bindparam/label, emitted form, `examples/users.csv` (same query as the demo), quoted CSV fields, unknown table, unsupported JOIN.

## License

MIT — see [LICENSE](LICENSE).
