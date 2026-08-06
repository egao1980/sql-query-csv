# sql-query-csv

Compile [`sql-query`](https://github.com/egao1980/sql-query) **SELECT** ASTs to **Lisp** that runs over CSV tables (in-memory plists or files).

| API | Target |
|-----|--------|
| `sql-query:compile-sql` | SQL text + params |
| `sql-query-csv:compile-query` | zero-arg function → plist rows |
| `sql-query-csv:compile-query-form` | inspectable `(lambda () …)` form |
| `sql-query-csv:query-csv` | compile + run |

## Supported

- single / multi-table `SELECT` with **`JOIN` / `LEFT JOIN` / `RIGHT JOIN` / `CROSS JOIN`**
- `WHERE`, `GROUP BY`, `HAVING`, aggregates `COUNT` / `SUM` / `AVG` / `MIN` / `MAX`
- `DISTINCT` (not `DISTINCT ON`)
- `ORDER BY`, `LIMIT` / `OFFSET`, projection / `label`
- table-qualified columns (`:users.id`)

Not yet: `FULL OUTER` / `NATURAL` join, CTE, DML, window functions.

## Example

```bash
ros -l examples/demo.lisp -q
```

Data: [`examples/users.csv`](examples/users.csv), [`examples/orders.csv`](examples/orders.csv).

```lisp
(asdf:load-system "sql-query-csv")
(use-package '(:sql-query :sql-query-csv))

(defparameter *d*
  (csv-catalog
   :users (merge-pathnames "examples/users.csv"
                           (asdf:system-source-directory "sql-query-csv"))
   :orders (merge-pathnames "examples/orders.csv"
                            (asdf:system-source-directory "sql-query-csv"))))

;; DISTINCT
(query-csv (select (distinct) (columns :city) (from :users) (order-by :city))
           :dialect *d*)

;; GROUP BY
(query-csv
 (select (columns :city (label (count :*) :n) (label (sql-func :sum :score) :total))
         (from :users)
         (where (:= :active 1))
         (group-by :city)
         (having (:> (count :*) 1)))
 :dialect *d*)

;; JOIN
(query-csv
 (select (columns :users.name :orders.item :orders.qty)
         (from :users)
         (join :orders (on (:= :users.id :orders.uid)))
         (order-by :users.name))
 :dialect *d*)

;; inspect emitted Lisp
(compile-query-form *stmt* :dialect *d*)
```

## Tests

```bash
ros -e '(asdf:test-system "sql-query-csv")' -q
```

## License

MIT — see [LICENSE](LICENSE).
