from __future__ import annotations
import sys
import os
from typing import Any, Dict

from .lreader import Reader, ReaderError
from .lcompiler import compile_module
from .builtins import (_pvec, _pmap, _pset, _plist, _kw, _sym, _ratio, loop, recur, tail_recursive, recurse, mutual,
                       # Type-preserving functional programming functions
                       map, cmap, emap, include, exclude, split, filter, remove, 
                       concat, take, drop, reverse, sort, unique, foldl, group_by, papply,
                       # Macro system
                       expand_macros, define_syntax_rules, parse_syntax_rules,
                       # Python import/export system
                       import_python_module, import_python_from, get_python_entity, get_python_module,
                       list_python_imports, export_to_python, create_python_module, list_lisp_exports,
                       # Dot operator
                       dot_access,
                       # I/O helpers
                       slurp, spit, read_lines, write_lines, with_open, read_stdin, read_stdin_lines)
from .symbol_table import get_all_symbols, add_symbol, get_symbol, has_symbol, list_symbols
import ast as A

# Set up readline for line editing
try:
    import readline
    import rlcompleter
    
    # Set up tab completion
    readline.parse_and_bind("tab: complete")
    
    # Set up history file
    histfile = os.path.join(os.path.expanduser("~"), ".lysp_history")
    try:
        readline.read_history_file(histfile)
        # Limit history to 1000 lines
        readline.set_history_length(1000)
    except (FileNotFoundError, PermissionError):
        pass
    
    def save_history():
        try:
            readline.write_history_file(histfile)
        except (PermissionError, OSError):
            pass
    
    # Custom completer for Lisp symbols
    class LispCompleter:
        def __init__(self):
            self.lisp_keywords = [
                'define', 'lambda', 'if', 'begin', 'let', 'set!', 'quote', 'quasiquote',
                'unquote', 'splice', 'true', 'false', 'nil'
            ]
        
        def complete(self, text, state):
            if state == 0:
                # This is the first time for this text, so build a match list.
                self.matches = []
                
                # Add Lisp keywords
                for keyword in self.lisp_keywords:
                    if keyword.startswith(text):
                        self.matches.append(keyword)
                
                # Add symbols from the symbol table
                all_symbols = get_all_symbols()
                for name in all_symbols.keys():
                    if name.startswith(text):
                        self.matches.append(name)
                
                # Add common Lisp functions
                lisp_functions = ['car', 'cdr', 'cons', 'list', 'append', 'map', 'filter', 'reduce']
                for func in lisp_functions:
                    if func.startswith(text):
                        self.matches.append(func)
                
                # Add type-preserving functional programming functions
                func_functions = ['map', 'cmap', 'emap', 'include', 'exclude', 'split', 'filter', 'remove',
                                'concat', 'take', 'drop', 'reverse', 'sort', 'unique']
                for func in func_functions:
                    if func.startswith(text):
                        self.matches.append(func)
            
            if state < len(self.matches):
                return self.matches[state]
            else:
                return None
    
    def setup_completer():
        completer = LispCompleter()
        readline.set_completer(completer.complete)
    
except ImportError:
    # readline not available (e.g., on Windows)
    def save_history():
        pass
    
    def setup_completer():
        pass

BANNER = """Proto Lisp→Python-AST REPL (split version). Forms: quote, if, begin, define, lambda, let, set!, application.
Data: (), [], {}, #{}, #(), strings, numbers, booleans, nil, :keywords.
Features: Tab completion, command history, line editing, tail recursion, type-preserving functional programming, R7RS hygienic macros, modern import/export syntax. Ctrl-D to exit.
"""

def _repr(v: Any) -> str:
    from .lreader import Keyword, Symbol
    if isinstance(v, Keyword): return f":{v.qual}"
    if isinstance(v, Symbol): return v.qual
    return repr(v)

def initialize_symbol_table():
    """Initialize the symbol table with all built-in functions and symbols"""
    # Data structure shims
    add_symbol("_pvec", _pvec)
    add_symbol("_pmap", _pmap)
    add_symbol("_pset", _pset)
    add_symbol("_plist", _plist)
    add_symbol("_kw", _kw)
    add_symbol("_sym", _sym)
    add_symbol("_ratio", _ratio)
    
    # Control flow
    add_symbol("loop", loop)
    add_symbol("recur", recur)
    add_symbol("tail_recursive", tail_recursive)
    add_symbol("recurse", recurse)
    add_symbol("mutual", mutual)
    
    # Type-preserving functional programming functions
    add_symbol("map", map)
    add_symbol("cmap", cmap)
    add_symbol("emap", emap)
    add_symbol("include", include)
    add_symbol("exclude", exclude)
    add_symbol("split", split)
    add_symbol("filter", filter)
    add_symbol("remove", remove)
    add_symbol("concat", concat)
    add_symbol("take", take)
    add_symbol("drop", drop)
    add_symbol("reverse", reverse)
    add_symbol("sort", sort)
    add_symbol("unique", unique)
    add_symbol("foldl", foldl)
    add_symbol("group_by", group_by)
    add_symbol("papply", papply)
    
    # Macro system
    add_symbol("expand_macros", expand_macros)
    add_symbol("define_syntax_rules", define_syntax_rules)
    add_symbol("parse_syntax_rules", parse_syntax_rules)
    
    # Python import/export system
    add_symbol("import_python_module", import_python_module)
    add_symbol("import_python_from", import_python_from)
    add_symbol("get_python_entity", get_python_entity)
    add_symbol("get_python_module", get_python_module)
    add_symbol("list_python_imports", list_python_imports)
    add_symbol("export_to_python", export_to_python)
    add_symbol("create_python_module", create_python_module)
    add_symbol("list_lisp_exports", list_lisp_exports)
    
    # Symbol table functions
    add_symbol("add_symbol", add_symbol)
    add_symbol("get_symbol", get_symbol)
    add_symbol("has_symbol", has_symbol)
    add_symbol("list_symbols", list_symbols)
    add_symbol("get_all_symbols", get_all_symbols)
    
    # Basic arithmetic and comparison
    add_symbol("+", lambda *xs: sum(xs))
    add_symbol("-", lambda a, *rest: (-a if not rest else (a - sum(rest))))
    add_symbol("*", lambda *xs: (1 if not xs else __import__("math").prod(xs)))
    add_symbol("<", lambda a, b: a < b)
    add_symbol("<=", lambda a, b: a <= b)
    add_symbol(">", lambda a, b: a > b)
    add_symbol(">=", lambda a, b: a >= b)
    add_symbol("=", lambda a, b: a == b)
    add_symbol("mod", lambda a, b: a % b)
    add_symbol("inc", lambda x: x + 1)
    add_symbol("not", lambda x: not x)
    
    # List operations
    add_symbol("list?", lambda x: isinstance(x, list))
    add_symbol("car", lambda x: x[0] if x else None)
    add_symbol("cdr", lambda x: x[1:] if x else [])
    add_symbol("print", lambda *xs: print(*xs))
    
    # Dot operator
    add_symbol("dot_access", dot_access)
    # I/O helpers
    add_symbol("slurp", slurp)
    add_symbol("spit", spit)
    add_symbol("read_lines", read_lines)
    add_symbol("write_lines", write_lines)
    add_symbol("with_open", with_open)
    add_symbol("read_stdin", read_stdin)
    add_symbol("read_stdin_lines", read_stdin_lines)

def run_repl():
    print(BANNER)
    
    # Initialize the symbol table with all built-ins
    initialize_symbol_table()
    
    # Set up custom completer
    setup_completer()
    
    buf = ""; prompt = "> "
    
    try:
        while True:
            try:
                line = input(prompt)
            except EOFError:
                print(); break
            buf += line + "\n"
            r = Reader(buf)
            try:
                forms = r.read()
            except ReaderError:
                prompt = "… "; continue
            prompt = "> "
            if not forms:
                buf = ""; continue
            try:
                # Execute each form individually to ensure imports are immediately available
                last_result = None
                for i, form in enumerate(forms):
                    # Compile the single form
                    mod = compile_module([form])
                    A.fix_missing_locations(mod)
                    code = compile(mod, filename="<repl>", mode="exec")
                    
                    # Always get a fresh copy of all symbols from the symbol table for execution
                    exec_env = get_all_symbols()
                    
                    # Execute the code
                    exec(code, exec_env, exec_env)
                    
                    # Update the symbol table with any new symbols from execution
                    for key, value in exec_env.items():
                        if not has_symbol(key):
                            add_symbol(key, value)
                    
                    # Store the result of this form
                    last_name = "__expr0"  # Each form gets __expr0 since we compile them individually
                    if last_name in exec_env:
                        last_result = exec_env[last_name]
                
                # Display the result of the last expression
                if last_result is not None:
                    # Handle different types of results
                    if callable(last_result):
                        # For function objects and objects with __call__ method, just display them without calling
                        if hasattr(last_result, '__name__'):
                            print(f"<function> {last_result.__name__}")
                        else:
                            print(f"<functor> {type(last_result).__name__}")
                    else:
                        # For atoms, just display them
                        print(_repr(last_result))
                        
            except Exception as ex:
                print(f"! Error: {ex}")
            buf = ""
    finally:
        # Save history when exiting
        save_history()

if __name__ == "__main__":
    if len(sys.argv) == 1:
        run_repl()
    else:
        with open(sys.argv[1], 'r', encoding='utf-8') as fh:
            src = fh.read()
        from .lreader import Reader
        r = Reader(src, file=sys.argv[1])
        forms = r.read()
        mod = compile_module(forms)
        code = compile(mod, filename=sys.argv[1], mode="exec")
        
        # Initialize symbol table for file execution
        initialize_symbol_table()
        exec_env = get_all_symbols()
        exec(code, exec_env, exec_env)