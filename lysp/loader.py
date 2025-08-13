from __future__ import annotations
import importlib.abc
import importlib.machinery
import sys
import os
from types import ModuleType
from typing import Optional


class LyspLoader(importlib.abc.Loader):
    """PEP 451 loader that compiles and executes .lysp/.lisp modules."""

    def __init__(self, origin_path: str) -> None:
        self.origin_path = origin_path

    def create_module(self, spec):
        # Use default module creation semantics
        return None

    def exec_module(self, module: ModuleType) -> None:
        # Lazy imports to avoid import-time cycles
        from .lreader import Reader
        from .lcompiler import compile_module
        # Seed module globals with lysp builtins expected by compiled code
        from . import builtins as b

        with open(self.origin_path, 'r', encoding='utf-8') as fh:
            src = fh.read()

        reader = Reader(src, file=self.origin_path)
        forms = reader.read()
        mod_ast = compile_module(forms)
        code = compile(mod_ast, filename=self.origin_path, mode='exec')

        # Populate required builtins into the module namespace
        seeds = {
            # Data structure shims
            '_pvec': b._pvec, '_pmap': b._pmap, '_pset': b._pset, '_plist': b._plist,
            '_kw': b._kw, '_sym': b._sym, '_ratio': b._ratio,
            # Control/tail recursion
            'loop': getattr(b, 'loop', None), 'recur': getattr(b, 'recur', None),
            'tail_recursive': getattr(b, 'tail_recursive', None),
            'recurse': getattr(b, 'recurse', None), 'mutual': getattr(b, 'mutual', None),
            # Type-preserving FP utilities
            'map': getattr(b, 'map', None), 'cmap': getattr(b, 'cmap', None), 'emap': getattr(b, 'emap', None),
            'include': getattr(b, 'include', None), 'exclude': getattr(b, 'exclude', None),
            'split': getattr(b, 'split', None), 'filter': getattr(b, 'filter', None),
            'remove': getattr(b, 'remove', None), 'concat': getattr(b, 'concat', None),
            'take': getattr(b, 'take', None), 'drop': getattr(b, 'drop', None),
            'reverse': getattr(b, 'reverse', None), 'sort': getattr(b, 'sort', None),
            'unique': getattr(b, 'unique', None),
            # Macro/import system helpers referenced by compiled code
            'import_python_module': getattr(b, 'import_python_module', None),
            'import_python_from': getattr(b, 'import_python_from', None),
            'get_python_entity': getattr(b, 'get_python_entity', None),
            'get_python_module': getattr(b, 'get_python_module', None),
            'list_python_imports': getattr(b, 'list_python_imports', None),
            'export_to_python': getattr(b, 'export_to_python', None),
            'create_python_module': getattr(b, 'create_python_module', None),
            'list_lisp_exports': getattr(b, 'list_lisp_exports', None),
            # Dot access function
            'dot_access': getattr(b, 'dot_access', None),
            # Basic arithmetic used in examples
            '+': (lambda *xs: sum(xs)),
            '-': (lambda a, *rest: (-a if not rest else (a - sum(rest)))),
            '*': (lambda *xs: (1 if not xs else __import__('math').prod(xs))),
            '<': (lambda a, b: a < b),
            '<=': (lambda a, b: a <= b),
            '>': (lambda a, b: a > b),
            '>=': (lambda a, b: a >= b),
            '=': (lambda a, b: a == b),
            'mod': (lambda a, b: a % b),
            'inc': (lambda x: x + 1),
            'not': (lambda x: not x),
            'print': (lambda *xs: print(*xs)),
        }
        for k, v in seeds.items():
            if v is not None:
                module.__dict__.setdefault(k, v)

        exec(code, module.__dict__, module.__dict__)


class LyspFinder(importlib.abc.MetaPathFinder):
    """Finder that locates .lysp/.lisp files as importable modules."""

    def __init__(self, exts: Optional[tuple[str, ...]] = None) -> None:
        self.exts = exts or ('.lysp', '.lisp', '.lsp')

    def find_spec(self, fullname: str, path: Optional[list[str]], target=None):
        parts = fullname.split('.')
        filename = parts[-1]
        search_paths = path or sys.path
        for base in search_paths:
            # package path
            pkg_dir = os.path.join(base, *parts[:-1]) if parts[:-1] else base
            for ext in self.exts:
                candidate = os.path.join(pkg_dir, filename + ext)
                if os.path.isfile(candidate):
                    loader = LyspLoader(candidate)
                    return importlib.machinery.ModuleSpec(fullname, loader, origin=candidate)
        return None


_installed = False

def install() -> None:
    """Install the LyspFinder into sys.meta_path if not already present."""
    global _installed
    if _installed:
        return
    # Avoid duplicate installation
    for finder in sys.meta_path:
        if isinstance(finder, LyspFinder):
            _installed = True
            return
    sys.meta_path.insert(0, LyspFinder())
    _installed = True


