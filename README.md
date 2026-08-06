# sql-query-csv

Compile [`sql-query`](https://github.com/egao1980/sql-query) **SELECT** ASTs to **Lisp** that runs over CSV tables (in-memory plists or files).

Not a SQL-string dialect — parallel to `compile-sql`:

| API | Target |
|-----|--------|
| `sql-query:compile-sql` | SQL text + params |
| `sql-query-csv:compile-query` | zero-arg function → plist rows |
| `sql-query-csv:compile-query-form` | inspectable `(lambda () …)` form |

## Wave-1

Single-table `SELECT` · `WHERE` (`= < > AND OR LIKE IN BETWEEN IS NULL` + arith) · `ORDER BY` · `LIMIT`/`OFFSET` · column projection / `label`.

No JOIN / GROUP BY / DISTINCT / CTE / DML.

## Quick use

```lisp
(asdf:load-system "sql-query-csv")

(defparameter *d*
  (sql-query-csv:csv-catalog
   :users #p"users.csv"))   ; or a list of plists

(sql-query-csv:query-csv
 (sql-query:select
  (sql-query:columns :id :name)
  (sql-query:from :users)
  (sql-query:where (sql-query:sql-and
                    (sql-query:|=| :active 1)
                    (sql-query:sql-like :name "%a%")))
  (sql-query:order-by :name)
  (sql-query:limit 10))
 :dialect *d*)
```


```lisp
;; see emitted code
(sql-query-csv:compile-query-form stmt :dialect *d*)
```

## License

MIT — see [LICENSE](LICENSE).
