# Lysp Cheatsheet

## Data & Literals
- Lists: `(1 2 3)` → plist
- Vectors: `[1 2 3]` → pvector
- Maps: `{:a 1 :b 2}` → pmap
- Sets: `#{1 2 3}` → pset
- Tuples: `#(1 2 3)` → Python tuple
- Quote: `'x`, `'(:a 1)`; Quasiquote: `` `(...) ``; Unquote: `,x`; Splice: `,@xs`
- Keywords: `:a`, `:user/id`

## Special Forms
- Define value: `(define x 42)`
- Define function: `(define (f a b) (+ a b))`
- Lambda: `(lambda (x) (* x x))`
- If: `(if test then [else])`
- Begin: `(begin e1 e2 ... en)` → eval all, return last
- Let: `(let ((x 1) (y 2)) body ...)`
- Set!: `(set! x 10)`
- Loop/recur: `(loop ((n 10) (acc 0)) (if (= n 0) acc (recur (- n 1) (+ acc n))))`

## Function Calls & Keyword Args
- `(f a b :kw1 v1 :kw2 v2)`
- `((. json dumps) {:a 1} :indent 2 :sort_keys true)`

## Python Interop
- Import: `(import math)`, `(import math :as m)`, `(import math :all)`
- From-import: `(import math (sqrt pi))`
- Builtins: `(import python)`, `(import python :all)`
- Dot access: `(. math sqrt)`, `(. obj attr subattr)`
- Call accessed member: `((. math sqrt) 16)`

## Class Helpers
- Construct: `(new Class a b :kw v)`
- Init: `(init obj a b :kw v)`

## Standard Macros
- Index: `(index coll i j ...)`
- let*: `(let* ((x 1) (y (+ x 2))) body ...)`
- cond: `(cond (test body ...) ... (else body ...))`
- Thread-first: `(-> x (f a b) (g c))`
- Thread-last: `(->> x (f a b) (g c))`
- for-each: `(for-each (x coll) body ...)`
- doseq: `(doseq ((x xs) (y ys) ...) body ...)`

## FP Utilities
- Map: `(map f coll)`, `(cmap f coll)`, `(emap f coll)`
- Filters: `(include pred coll)`, `(exclude pred coll)`, `(filter pred coll)`, `(remove pred coll)`
- Sequence: `(concat a b)`, `(take n coll)`, `(drop n coll)`, `(reverse coll)`, `(sort coll [key])`, `(unique coll)`
- Folds/Grouping/Partial: `(foldl f init coll)`, `(group_by coll [k] [v])`, `(papply f args...)`

## I/O Helpers
- Read: `(read path)` / `(slurp path)`
- Write: `(write path data)` / `(spit path data [encoding append?])`
- Lines: `(read_lines path)`, `(write_lines path seq)`
- With open: `(with_open path "w" (lambda (fh) (print fh)))`
- Stdin: `(read_stdin)`, `(read_stdin_lines)`

## Common Patterns
- `(import math) ((. math sqrt) 9)` → `3.0`
- `(import numpy :as np) (index ((. np array) [[1 2] [3 4]]) 1 0)` → `3`
- Threading:
  - `(-> 5 (list) (concat [6 7]))`
  - `(->> [1 2 3] (map inc) (take 2))`
- Iteration:
  - `(for-each (x [1 2 3]) (print x))`
  - `(doseq ((x [1 2]) (y [10 20])) (print (+ x y)))`

## Notes
- Use `->` when the threaded value is the first argument; `->>` when last (sequence ops).
- Dot operator returns attributes; wrap to call.
- `:all` binds bare names; use carefully in large scopes.
