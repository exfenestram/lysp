# Lisp Compiler Manual

## Overview

This is a Lisp-to-Python compiler that translates Lisp code into Python AST (Abstract Syntax Tree) and executes it. The compiler supports a rich set of features including hygienic macros, type-preserving functional programming utilities, immutable data structures, Python interop (including a pseudo-module for Python builtins), a dot operator for attribute access, and keyword arguments when calling Python functions.

## Table of Contents

1. [Basic Syntax](#basic-syntax)
2. [Data Structures](#data-structures)
3. [Special Forms](#special-forms)
4. [Functional Programming](#functional-programming)
5. [Macro System](#macro-system)
6. [Python Integration](#python-integration)
7. [Function Application and Keyword Arguments](#function-application-and-keyword-arguments)
8. [Tail Recursion](#tail-recursion)
9. [Examples](#examples)
10. [REPL Usage](#repl-usage)

## Basic Syntax

### Comments
```lisp
; Single line comment
#| Multi-line comment |#
```

### Literals
```lisp
42          ; Integer
3.14        ; Float
"hello"     ; String
true        ; Boolean
false       ; Boolean
nil         ; Null value
```

### Symbols and Keywords
```lisp
symbol      ; Symbol
:keyword    ; Keyword
```

## Data Structures

### Lists
```lisp
()          ; Empty list
(1 2 3)     ; List of numbers
(a b c)     ; List of symbols
```

Lists are compiled to `pyrsistent.plist` for immutability.

### Vectors
```lisp
[]          ; Empty vector
[1 2 3]     ; Vector of numbers
[a b c]     ; Vector of symbols
```

Vectors are compiled to `pyrsistent.pvector`.

### Maps
```lisp
{}          ; Empty map
{:a 1 :b 2} ; Map with key-value pairs
```

Maps are compiled to `pyrsistent.pmap`.

### Sets
```lisp
#{}         ; Empty set
#{1 2 3}    ; Set of numbers
```

Sets are compiled to `pyrsistent.pset`.

### Tuples
```lisp
#()         ; Empty tuple
#(1 2 3)    ; Tuple of numbers
```

Tuples are compiled to Python tuples.

### Quoting
```lisp
'form       ; Quote (syntactic sugar for (quote form))
`form       ; Quasiquote
,form       ; Unquote
,@form      ; Splice-unquote
```

Quoted lists produce immutable `plist` values.

## Special Forms

### define
```lisp
(define name value)                    ; Define variable
(define (function-name args...) body)  ; Define function
```

Examples:
```lisp
(define x 42)
(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))
```

### lambda
```lisp
(lambda (args...) body...)
```

Examples:
```lisp
(lambda (x) (* x 2))
(lambda (x y) (+ x y))
```

### if
```lisp
(if test then [else])
```

Examples:
```lisp
(if (> x 0) "positive" "negative")
(if (= x 0) 1 (* x (factorial (- x 1))))
```

### begin
```lisp
(begin expr...)
```

Evaluates expressions in sequence, returns the last value.

### let
```lisp
(let ((var1 val1) (var2 val2)...) body...)
```

Examples:
```lisp
(let ((x 1) (y 2)) (+ x y))
```

### set!
```lisp
(set! variable value)
```

Assigns a new value to a variable.

### loop/recur
```lisp
(loop bindings body...)
```

Tail-recursive loop with `recur` for iteration.

Examples:
```lisp
(loop ((n 10) (acc 1))
  (if (= n 0) acc (recur (- n 1) (* n acc))))
```

## Functional Programming

The compiler includes a comprehensive set of functional programming utilities that preserve the type of their input data structures.

### Mapping Functions
```lisp
(map function collection)           ; Map function over collection
(cmap function collection)          ; Map with exception catching
(emap function collection)          ; Map returning exceptions
```

Examples:
```lisp
(map inc '(1 2 3))     ; → plist([2, 3, 4])
(map inc [1 2 3])      ; → pvector([2, 3, 4])
(map inc #{1 2 3})     ; → pset([2, 3, 4])
(map inc #(1 2 3))     ; → (2, 3, 4)
```

### Filtering Functions
```lisp
(include predicate collection)      ; Include elements matching predicate
(exclude predicate collection)      ; Exclude elements matching predicate
(filter predicate collection)       ; Filter elements
(remove predicate collection)       ; Remove elements
```

Examples:
```lisp
(define even? (lambda (x) (= (mod x 2) 0)))
(include even? '(1 2 3 4 5))       ; → plist([2, 4])
```

### Sequence Functions
```lisp
(take n collection)                 ; Take first n elements
(drop n collection)                 ; Drop first n elements
(reverse collection)                ; Reverse collection
(sort collection [key-function])    ; Sort collection
(unique collection)                 ; Remove duplicates
(concat collection1 collection2)    ; Concatenate collections
```

### Splitting and Grouping
```lisp
(split predicate collection)        ; Split collection by predicate
```

### Additional Helpers
```lisp
(foldl f init coll)            ; left fold, returns accumulator
(group_by coll [key-fn] [val-fn]) ; returns Python dict key -> [vals]
(papply f args...)             ; partial application, returns function
```

Examples:
```lisp
(foldl + 0 [1 2 3 4])               ; → 10
(group_by '(a b a c) (lambda (x) x)) ; → {"a": ['a 'a], "b": ['b], "c": ['c]}
(((papply + 1 2) 3))                 ; → 6
```

## Macro System

The compiler implements R7RS-compliant hygienic macros using `syntax-rules`.

### Standard Macros (pre-installed)

- `index`: generalized indexing via `__getitem__`
  - `(index coll i)` → `coll[i]`
  - `(index coll i j ...)` → `coll[i][j]...`
  - Works for vectors, lists, tuples, maps, numpy arrays

- `let*`: sequential bindings
  - `(let* ((x v) (y w) ...) body ...)` ⇒ nested `let`

- `cond`: conditional chains with `else`
  - `(cond (test body ...) ... (else body ...))`

- Threading macros:
  - `->` thread-first: `(-> x (f a b) (g c))` ⇒ `(g (f x a b) c)`
  - `->>` thread-last: `(->> x (f a b) (g c))` ⇒ `(g c (f a b x))`

- Iteration macros:
  - `for-each`: `(for-each (x coll) body ...)` strict order, side effects ok
  - `doseq`: nested iteration `(doseq ((x xs) (y ys) ...) body ...)`

Examples:
```lisp
(index [10 20 30] 1)                 ; → 20
(let* ((x 1) (y (+ x 2))) (* x y))   ; → 3
(-> [1 2 3] (map inc) (take 2))      ; thread-first for first-arg APIs
(->> [1 2 3] (map inc) (take 2))     ; thread-last for collection-last APIs
(for-each (x [1 2 3]) (print x))     ; prints 1,2,3
(doseq ((x [1 2]) (y [10 20])) (print (+ x y))) ; prints 11,21,12,22
```

## Python Integration

### Importing Python into Lisp

The new import syntax provides a more Lisp-like and flexible way to import Python modules and entities.

#### Module Imports
```lisp
(import module-name)
(import module-name :as alias)
(import module-name :all)          ; bind all public names as bare symbols too
```

Examples:
```lisp
(import math)
((. math sqrt) 16)                 ; → 4.0
(import math :all)
(sqrt 16)                          ; → 4.0 (bare name via :all)
```

#### From Imports
```lisp
(import module-name (entity1 entity2 ...))
(import module-name (entity1 entity2 ...) :as alias)
```

Examples:
```lisp
(import math (sqrt pi))
((. math sqrt) 9)                  ; → 3.0
```

#### Dot Operator (Attribute Access)
```lisp
(. object member1 member2 ...)
```
- Returns the nested attribute without calling it
- Works with modules, objects, and results of expressions

Examples:
```lisp
(import math)
(. math pi)                ; → 3.141592653589793
(. math sqrt)              ; → <function> sqrt
((. math sqrt) 16)         ; → 4.0

(import numpy :as np)
((. np array) [1 2 3])     ; construct a NumPy array
```

#### Python Pseudo-Module
```lisp
(import python)
(. python print)           ; -> <function> print
((. python len) [1 2 3])   ; -> 3
(import python :all)
(len [1 2 3 4])            ; -> 4  (bare name via :all)
```

Notes:
- Import binds the module symbol (e.g., `math`) and all qualified names (e.g., `math.sqrt`).
- Using `:all` also binds public names as bare symbols (e.g., `sqrt`).
- The result of `(. ...)` is not called automatically; wrap in `((. ...) args...)` to invoke.

### Class Construction and __init__

Two convenience forms are available:
- `new` instantiates a class with positional and keyword args.
- `init` explicitly calls an instance’s `__init__`.

Examples:
```lisp
(new Point 1 2 :color :red)
(init obj :verbose true)
```

## Function Application and Keyword Arguments

You can call Python (and Lisp) functions using positional and keyword arguments.

- Positional arguments first, followed by keyword pairs `:name value`.
- Example:
```lisp
(import json)
((. json dumps) {"a" 1} :indent 2 :sort_keys true)
; Compiles to: json.dumps({"a": 1}, indent=2, sort_keys=True)
```

## Tail Recursion

The compiler can optimize tail-recursive functions using a continuation-based system compatible with Adamantine’s tail recursion utilities.

### Automatic Detection
Functions that call `recur` in a tail position are optimized.

### Manual Tail Recursion
```lisp
(define factorial
  (lambda (n)
    (loop ((n n) (acc 1))
      (if (= n 0) 
          acc 
          (recur (- n 1) (* n acc))))))
```

## Examples

### Basic Examples

#### Hello World
```lisp
(print "Hello, World!")
```

#### Factorial Function
```lisp
(define factorial
  (lambda (n)
    (if (= n 0) 1 (* n (factorial (- n 1))))))

(print (factorial 5))  ; → 120
```

#### List Processing
```lisp
(define numbers '(1 2 3 4 5))
(define doubled (map (lambda (x) (* x 2)) numbers))
(define evens (include (lambda (x) (= (mod x 2) 0)) numbers))
(print doubled)  ; → plist([2, 4, 6, 8, 10])
(print evens)    ; → plist([2, 4])
```

### Python Integration
```lisp
(import math :all)           ; bind sqrt and pi as bare names
(print (sqrt 16))            ; 4.0
(print pi)                   ; 3.141592653589793

(import python :all)
(print (len [1 2 3]))        ; 3
```

### Macro Examples
```lisp
; Define a macro for list comprehensions
(define-syntax list-comp
  (syntax-rules ()
    ((list-comp expr for var in collection)
     (map (lambda (var) expr) collection))))

; Use the macro
(define squares (list-comp (* x x) for x in '(1 2 3 4 5)))
(print squares)  ; → plist([1, 4, 9, 16, 25])
```

## I/O Helpers

Convenience functions for file and stdin I/O.

```lisp
(read path [encoding])            ; read entire file (alias of slurp)
(write path data [encoding])      ; write data (overwrite) (alias of spit)
(slurp path [encoding])
(spit path data [encoding append?])
(read_lines path [encoding])      ; → vector of lines
(write_lines path seq [encoding newline append?])
(with_open path mode (lambda (fh) ...))
(read_stdin [encoding])           ; read all stdin into string
(read_stdin_lines [encoding])     ; vector of stdin lines
```

Examples:
```lisp
(write "out.txt" "hello")
(print (read "out.txt"))
(write_lines "nums.txt" [1 2 3])
(for-each (ln (read_lines "nums.txt")) (print ln))
```

## REPL Usage

### Starting the REPL
```bash
python3 -m lysp.lrepl
```

### REPL Features
- **Tab Completion**: Auto-complete for symbols and functions
- **Command History**: Navigate with arrow keys
- **Line Editing**: Full line editing capabilities
- **Multi-line Input**: Automatic continuation with `...`

### REPL Commands
```lisp
; Basic evaluation
(+ 1 2)                    ; → 3

; Define and use functions
(define (square x) (* x x))
(square 5)                 ; → 25

; Import Python modules and use dot operator
(import math)
((. math sqrt) 16)         ; → 4.0
(. math pi)                ; → 3.141592653589793

; Keyword arguments to Python functions
(import json)
((. json dumps) {"a" 1} :indent 2 :sort_keys true)

; Use macros
(when true (print "hello")) ; → hello

; Export to Python
(define lisp-func (lambda (x) (* x 2)))
(export lisp-func)
(create_python_module "my_module")
```

### File Execution
```bash
python3 -m lysp.lrepl filename.lisp
```

## Data Type Preservation

The functional programming utilities preserve the type of their input:

```lisp
; Lists remain lists
(map inc '(1 2 3))     ; → plist([2, 3, 4])

; Vectors remain vectors  
(map inc [1 2 3])      ; → pvector([2, 3, 4])

; Sets remain sets
(map inc #{1 2 3})     ; → pset([2, 3, 4])

; Tuples remain tuples
(map inc #(1 2 3))     ; → (2, 3, 4)
```

## Error Handling

### Common Errors
- **Undefined Variable**: `! Error: name 'x' is not defined`
- **Wrong Number of Arguments**: `! Error: function expected 2 arguments, got 3`
- **Import Error**: `! Error: Could not import module 'nonexistent'`
- **Macro Error**: `! Error: No matching rule found for macro 'macro-name'`

### Debugging Tips
1. Use `print` to inspect values
2. Check function argument counts
3. Verify import module names
4. Test macro patterns step by step

## Performance Considerations

1. **Immutable Data**: All data structures use `pyrsistent` for immutability
2. **Tail Recursion**: Optimized with continuation-based approach
3. **Macro Expansion**: Happens at compile time
4. **Python Integration**: Direct access to Python objects via imports and dot operator

## Best Practices

1. **Use Tail Recursion**: For iterative algorithms
2. **Leverage Macros**: For domain-specific syntax and abstractions
3. **Type Preservation**: Use appropriate data structures
4. **Python Integration**: `:all` for convenience; dot for explicit attribute access
5. **Functional Style**: Prefer pure functions and immutable data

## Conclusion

This Lisp compiler provides a powerful, modern Lisp implementation with:
- Full R7RS hygienic macro system
- Comprehensive type-preserving functional programming utilities
- Python interop (modules, from-imports, dot access, pseudo-module of builtins, keyword args)
- Tail recursion support
- Immutable data structures
- Rich REPL environment

It bridges Lisp's expressiveness with Python's ecosystem for seamless interop and productivity.
