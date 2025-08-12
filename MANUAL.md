# Lisp Compiler Manual

## Overview

This is a Lisp-to-Python compiler that translates Lisp code into Python AST (Abstract Syntax Tree) and executes it. The compiler supports a rich set of features including hygienic macros, functional programming utilities, and bidirectional Python integration.

## Table of Contents

1. [Basic Syntax](#basic-syntax)
2. [Data Structures](#data-structures)
3. [Special Forms](#special-forms)
4. [Functional Programming](#functional-programming)
5. [Macro System](#macro-system)
6. [Python Integration](#python-integration)
7. [Tail Recursion](#tail-recursion)
8. [Examples](#examples)
9. [REPL Usage](#repl-usage)

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

## Macro System

The compiler implements R7RS-compliant hygienic macros using `syntax-rules`.

### define-syntax
```lisp
(define-syntax macro-name
  (syntax-rules (literals...)
    ((pattern1) template1)
    ((pattern2) template2)
    ...))
```

### Pattern Matching
- **Literals**: Symbols that must match exactly
- **Variables**: Symbols that bind to input
- **Ellipsis**: `...` for variable-length patterns

### Template Expansion
- **Variables**: Replaced with bound values
- **Literals**: Preserved as-is
- **Ellipsis**: Expand to multiple elements
- **Hygiene**: Automatic identifier renaming

### Built-in Macros

#### when
```lisp
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(when true (print "hello") (print "world"))
```

#### unless
```lisp
(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...)))))

(unless false (print "unless works"))
```

#### cond
```lisp
(define-syntax cond
  (syntax-rules (else)
    ((cond (else body ...))
     (begin body ...))
    ((cond (test body ...) rest ...)
     (if test (begin body ...) (cond rest ...)))))

(cond
  (false (print "first"))
  (true (print "second"))
  (else (print "else")))
```

#### and
```lisp
(define-syntax and
  (syntax-rules ()
    ((and) true)
    ((and test) test)
    ((and test rest ...)
     (if test (and rest ...) false))))

(and true true false)  ; → false
```

## Python Integration

### Importing Python into Lisp

The new import syntax provides a more Lisp-like and flexible way to import Python modules and entities.

#### Module Imports
```lisp
(import module-name)
(import module-name :as alias)
```

Examples:
```lisp
(import math)
(import numpy :as np)
```

#### From Imports
```lisp
(import module-name (entity1 entity2 ...))
(import module-name (entity1 :as alias1 entity2 :as alias2 ...))
```

Examples:
```lisp
(import math (sqrt pi))
(import numpy (array :as arr zeros :as z))
```

#### Accessing Imported Entities (Dot Operator)
After importing, you can access module members with the dot operator:
```lisp
(. object member1 member2 ...)
```
- Returns the nested attribute without calling it
- Works with modules, objects, and results of expressions

Examples:
```lisp
(import math)
(. math pi)                 ; → 3.141592653589793
(. math sqrt)               ; → <function> sqrt
((. math sqrt) 16)          ; → 4.0

(import numpy :as np)
((. np array) [1 2 3])      ; construct a NumPy array
```

Notes:
- The result of `(. ...)` is not called automatically; it returns the object or function.
- Use the enclosing call syntax to invoke returned callables: `((. module func) args...)`.
- The symbol table is the single source of truth for imported symbols; imports are immediately available in the REPL.

### Exporting Lisp to Python

#### Export Functions/Data
```lisp
(export name)
(export name :as python-name)
(export name :to module-name)
```

Examples:
```lisp
(define factorial (lambda (n) 
  (if (= n 0) 1 (* n (factorial (- n 1))))))
(export factorial :as fact)
(export factorial :to math_utils)
```

#### Create Python Modules
```lisp
(create_python_module "module-name")
```

Examples:
```lisp
(define double (lambda (x) (* x 2)))
(export double)
(create_python_module "lisp_utils")
```

## Tail Recursion

The compiler automatically detects and optimizes tail-recursive functions using the `adamantine` library.

### Automatic Detection
Functions that call `recur` are automatically wrapped with `@tail_recursive`.

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

### Advanced Examples

#### Python Integration
```lisp
; Import Python libraries
(import math (sqrt pi))
(import numpy :as np)

; Use Python functions
(define radius 5)
(define area (* pi (* radius radius)))
(print area)  ; → 78.53981633974483

; Export Lisp functions to Python
(define lisp-factorial
  (lambda (n)
    (if (= n 0) 1 (* n (lisp-factorial (- n 1))))))

(export lisp-factorial :as factorial)
(create_python_module "lisp_math")
```

#### Macro Examples
```lisp
; Define a macro for list comprehensions
(define-syntax list-comp
  (syntax-rules ()
    ((list-comp expr for var in collection)
     (map (lambda (var) expr) collection))))

; Use the macro
(define squares (list-comp (* x x) for x in '(1 2 3 4 5)))
(print squares)  ; → plist([1, 4, 9, 16, 25])

; Define a macro for pattern matching
(define-syntax match
  (syntax-rules ()
    ((match value
       (pattern1 result1)
       (pattern2 result2)
       ...)
     (cond
       ((equal? value pattern1) result1)
       ((equal? value pattern2) result2)
       ...))))

; Use pattern matching
(define result (match 2
  (1 "one")
  (2 "two")
  (else "other")))
(print result)  ; → "two"
```

#### Functional Programming
```lisp
; Pipeline processing
(define data '(1 2 3 4 5 6 7 8 9 10))
(define result (->> data
                   (filter (lambda (x) (> x 5)))
                   (map (lambda (x) (* x x)))
                   (take 3)))
(print result)  ; → plist([36, 49, 64])

; Higher-order functions
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define add1-then-double (compose double add1))
(print (add1-then-double 5))  ; → 12
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

1. **Immutable Data**: All data structures are immutable using `pyrsistent`
2. **Tail Recursion**: Automatically optimized for stack efficiency
3. **Macro Expansion**: Happens at compile time, no runtime overhead
4. **Python Integration**: Direct access to Python objects, minimal overhead

## Best Practices

1. **Use Tail Recursion**: For iterative algorithms to avoid stack overflow
2. **Leverage Macros**: For domain-specific syntax and abstractions
3. **Type Preservation**: Use appropriate data structures for your use case
4. **Python Integration**: Import only what you need, export reusable functions
5. **Functional Style**: Prefer pure functions and immutable data

## Conclusion

This Lisp compiler provides a powerful, modern Lisp implementation with:
- Full R7RS hygienic macro system
- Comprehensive functional programming utilities
- Bidirectional Python integration
- Automatic tail recursion optimization
- Immutable data structures
- Rich REPL environment

The compiler bridges the gap between Lisp's expressiveness and Python's ecosystem, making it ideal for both Lisp programming and Python integration scenarios.
