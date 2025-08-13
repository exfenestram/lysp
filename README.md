# lysp Lisp Compiler

The lysp Lisp compiler is a Schemish Lisp compiler which generates code for the Python AST. Thus it is fully interoperable with Python.

Native data structures are fully immutable. Lysp defines the following structured data types
List
Vector
Tuple
Set
Map

All of these data structures are immutable, and implemented by the Pyrsistent library

Tail recursion is fully supported with the "recur" keyword.


## Simple import/export syntax
To import, use the forms

(import module)
(import module :as name)
(import module :all)
(import module (list-of-imports))
(import module (name1 :as alias1 ...))

To export, use
(export funcname)
or
(export funcname :to module)
or 
(export funcname :as name)

The export line should occur after the definition of the function

Python modules or Lysp modules may be imported.
