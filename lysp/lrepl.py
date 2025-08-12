from __future__ import annotations
import sys
import os
from typing import Any, Dict

from .lreader import Reader, ReaderError
from .lcompiler import compile_module
from .builtins import (_pvec, _pmap, _pset, _plist, _kw, _sym, _ratio, loop, recur, tail_recursive, recurse, mutual,
                       # Type-preserving functional programming functions
                       map, cmap, emap, include, exclude, split, filter, remove, 
                       concat, take, drop, reverse, sort, unique,
                       # Macro system
                       expand_macros, define_syntax_rules, parse_syntax_rules,
                       # Python import/export system
                       import_python_module, import_python_from, get_python_entity, get_python_module,
                       list_python_imports, export_to_python, create_python_module, list_lisp_exports,
                       add_to_global_env, get_from_global_env, list_global_env, get_imported)
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
        def __init__(self, env):
            self.env = env
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
                
                # Add environment variables
                for name in self.env.keys():
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
    
    def setup_completer(env):
        completer = LispCompleter(env)
        readline.set_completer(completer.complete)
    
except ImportError:
    # readline not available (e.g., on Windows)
    def save_history():
        pass
    
    def setup_completer(env):
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


def run_repl():
    print(BANNER)
    # Get initial global environment
    global_env = list_global_env()
    
    env: Dict[str, Any] = {
            "_pvec": _pvec,
    "_pmap": _pmap,
    "_pset": _pset,
    "_plist": _plist,
        "_kw": _kw,
        "_sym": _sym,
        "_ratio": _ratio,
        "loop": loop,
        "recur": recur,
        "tail_recursive": tail_recursive,
        "recurse": recurse,
        "mutual": mutual,
        # Type-preserving functional programming functions
        "map": map,
        "cmap": cmap,
        "emap": emap,
        "include": include,
        "exclude": exclude,
        "split": split,
        "filter": filter,
        "remove": remove,
        "concat": concat,
        "take": take,
        "drop": drop,
        "reverse": reverse,
        "sort": sort,
        "unique": unique,
        # Macro system
        "expand_macros": expand_macros,
        "define_syntax_rules": define_syntax_rules,
        "parse_syntax_rules": parse_syntax_rules,
        # Python import/export system
        "import_python_module": import_python_module,
        "import_python_from": import_python_from,
        "get_python_entity": get_python_entity,
        "get_python_module": get_python_module,
        "list_python_imports": list_python_imports,
        "export_to_python": export_to_python,
        "create_python_module": create_python_module,
        "list_lisp_exports": list_lisp_exports,
        "add_to_global_env": add_to_global_env,
        "get_from_global_env": get_from_global_env,
        "list_global_env": list_global_env,
        "get_imported": get_imported,
        # Tiny prelude
        "+": lambda *xs: sum(xs),
        "-": lambda a, *rest: (-a if not rest else (a - sum(rest))),
        "_": lambda a, *rest: (-a if not rest else (a - sum(rest))),  # Mangled version
        "*": lambda *xs: (1 if not xs else __import__("math").prod(xs)),
        "<": lambda a, b: a < b,
        "<=": lambda a, b: a <= b,
        ">": lambda a, b: a > b,
        ">=": lambda a, b: a >= b,
        "=": lambda a, b: a == b,
        "mod": lambda a, b: a % b,
        "inc": lambda x: x + 1,
        "not": lambda x: not x,
        "list?": lambda x: isinstance(x, list),
        "car": lambda x: x[0] if x else None,
        "cdr": lambda x: x[1:] if x else [],
        "print": lambda *xs: print(*xs),
    }
    
    # Add global environment to the REPL environment
    env.update(global_env)
    
    # Set up custom completer
    setup_completer(env)
    
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
                mod = compile_module(forms)
                A.fix_missing_locations(mod)
                code = compile(mod, filename="<repl>", mode="exec")
                
                # Always include the latest global environment
                global_env = list_global_env()
                combined_env = env.copy()
                combined_env.update(global_env)
                
                exec(code, combined_env, combined_env)
                
                # Update the original environment with any new bindings
                for key, value in combined_env.items():
                    if key not in env or env[key] is not combined_env[key]:
                        env[key] = combined_env[key]
                
                last_name = f"__expr{len(forms) - 1}"
                if last_name in combined_env:
                    print(_repr(combined_env[last_name]))
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
        print('FORMS', forms)
        mod = compile_module(forms)
        code = compile(mod, filename=sys.argv[1], mode="exec")
        env = {"_pvec": _pvec, "_pmap": _pmap, "_pset": _pset, "_plist": _plist, "_kw": _kw, "_sym": _sym, "_ratio": _ratio, 
               "loop": loop, "recur": recur, "tail_recursive": tail_recursive, "recurse": recurse, "mutual": mutual,
                               # Type-preserving functional programming functions
                "map": map, "cmap": cmap, "emap": emap, "include": include, "exclude": exclude, "split": split,
                "filter": filter, "remove": remove, "concat": concat, "take": take, "drop": drop, "reverse": reverse,
                "sort": sort, "unique": unique,
                # Macro system
                "expand_macros": expand_macros, "define_syntax_rules": define_syntax_rules, "parse_syntax_rules": parse_syntax_rules,
                # Python import/export system
                "import_python_module": import_python_module, "import_python_from": import_python_from,
                "get_python_entity": get_python_entity, "get_python_module": get_python_module,
                "list_python_imports": list_python_imports, "export_to_python": export_to_python,
                "create_python_module": create_python_module, "list_lisp_exports": list_lisp_exports}
        exec(code, env, env)