from __future__ import annotations
from fractions import Fraction
from typing import Any, List, Tuple, Dict

from .lreader import Symbol, _sym, _kw
from .lisp_adamantine import (lisp_map, lisp_cmap, lisp_emap, lisp_include, lisp_exclude, 
                             lisp_split, lisp_filter, lisp_remove, lisp_concat, lisp_take, 
                             lisp_drop, lisp_reverse, lisp_sort, lisp_unique,
                             lisp_foldl, lisp_group_by, lisp_papply)
from .macros import expand_macros, define_syntax_rules, parse_syntax_rules
from .python_imports import (import_python_module, import_python_from, get_python_entity,
                                   get_python_module, list_python_imports, export_to_python,
                                   create_python_module, list_lisp_exports)
from .symbol_table import add_symbol, get_symbol, has_symbol, list_symbols
from pyrsistent import pvector, pmap, pset, plist, l

# Reify lisp_* functions without the lisp_ prefix
map = lisp_map
cmap = lisp_cmap
emap = lisp_emap
include = lisp_include
exclude = lisp_exclude
split = lisp_split
filter = lisp_filter
remove = lisp_remove
concat = lisp_concat
take = lisp_take
drop = lisp_drop
reverse = lisp_reverse
sort = lisp_sort
unique = lisp_unique
foldl = lisp_foldl
group_by = lisp_group_by
papply = lisp_papply

__all__ = [
    "_pvec", "_pmap", "_pset", "_plist", "_ratio", "_kw", "_sym", "mangle_symbol",
    "loop", "recur", "tail_recursive", "recurse", "mutual",
    # Type-preserving functional programming functions
    "map", "cmap", "emap", "include", "exclude", "split", "filter", "remove", 
    "concat", "take", "drop", "reverse", "sort", "unique", "foldl", "group_by", "papply",
    # Macro system
    "expand_macros", "define_syntax_rules", "parse_syntax_rules",
    # Python import/export system
    "import_python_module", "import_python_from", "get_python_entity", "get_python_module",
    "list_python_imports", "export_to_python", "create_python_module", "list_lisp_exports",
               "add_symbol", "get_symbol", "has_symbol", "list_symbols",
    # I/O
    "slurp", "spit", "read_lines", "write_lines", "with_open", "read_stdin", "read_stdin_lines",
    "read", "write"
]

# Persistent data shims
try:
    from pyrsistent import pvector as _pvector, pmap as _pmap_impl, pset as _pset_impl, plist as _plist_impl
    def _pvec(xs): return _pvector(xs)
    def _pmap(pairs): return _pmap_impl(dict(pairs))
    def _pset(xs): return _pset_impl(xs)
    def _plist(xs): return _plist_impl(xs)
except Exception:
    from types import MappingProxyType
    def _pvec(xs): return tuple(xs)
    def _pmap(pairs): return MappingProxyType(dict(pairs))
    def _pset(xs): return frozenset(xs)
    def _plist(xs): return tuple(xs)


def mangle_symbol(sym: Symbol) -> str:
    name = sym.qual.replace('/', '__').replace('-', '_')
    if name in {"def","class","return","if","else","for","while","try","except","lambda"}:
        name += "_"
    return name


def _ratio(n: int, d: int) -> Fraction:
    return Fraction(n, d)


# Tail recursion and optional utilities from adamantine (do not shadow Lisp FP fns)
try:
    from adamantine.tail_recursive import tail_recursive, recurse, mutual
    # Import non-conflicting helpers only (avoid map/include/etc. which we provide via lisp_adamantine)
    from adamantine.exec_models import (
        foldl as _adam_foldl, groupby, group_count, groupby_set, merge, is_empty, pairwise, pairwise_chain,
        map_iter, cmap_iter, emap_iter, pairwise_iter, pairwise_chain_iter, walk, multi_foldl,
        group_by_index,
    )
    from adamantine.time_exec import time_exec
    from adamantine.apply import apply, apply_iter
    from adamantine.partial import papply as _adam_papply 
    from adamantine.predicates import complement, all_of, some_of
    from adamantine.lru_cache import cached, LRUCache, clear_cache, get_cache
    from adamantine.statistics import Deviator, apply_sequence, empty_deviator, zscore
    ADAMANTINE_AVAILABLE = True
except ImportError:
    # Fallback implementation if adamantine is not available
    from pyrsistent import plist, l
    
    
    
    ADAMANTINE_AVAILABLE = False
    
    # Fallback implementations for adamantine functions
    from pyrsistent import pvector
    

    
    def foldl(f, initializer, *args):
        """Left fold over sequences"""
        for item in zip(*args):
            initializer = f(initializer, *item)
        return initializer
    
    def multi_foldl(f, initializer, *args):
        """Multi-argument left fold over sequences"""
        for item in zip(*args):
            initializer = f(initializer, *item)
        return initializer
    
    def groupby(data, key=lambda x: x, value=lambda x: x):
        """Group data by key function"""
        result = {}
        for item in data:
            k = key(item)
            if k not in result:
                result[k] = []
            result[k].append(value(item))
        return result
    
    def groupby_set(data, key=lambda x: x, value=lambda x: x):
        """Group data by key function, using sets for values"""
        result = {}
        for item in data:
            k = key(item)
            if k not in result:
                result[k] = set()
            result[k].add(value(item))
        return result
    
    def group_count(data, key=lambda x: x):
        """Count occurrences by key function"""
        result = {}
        for item in data:
            k = key(item)
            result[k] = result.get(k, 0) + 1
        return result
    
    def group_by_index(data, group_size, even_split=True):
        """Group data by index into groups of specified size"""
        result = []
        for i in range(0, len(data), group_size):
            result.append(data[i:i+group_size])
        return result
    
    def merge(it1, it2, key=lambda x: x, reverse=False):
        """Merge two sorted iterables"""
        return sorted(list(it1) + list(it2), key=key, reverse=reverse)
    
    def is_empty(container):
        """Check if container is empty"""
        return not container
    
    def pairwise(comp_func, cmp, argslist):
        """Apply comparison function to pairs"""
        return [comp_func(cmp, a, b) for a, b in zip(argslist, argslist[1:])]
    
    def pairwise_chain(comp_func, argslist):
        """Apply comparison function to chained pairs"""
        return [comp_func(a, b) for a, b in zip(argslist, argslist[1:])]
    
    def walk(func, collection):
        """Walk through collection applying function"""
        if isinstance(collection, (list, tuple)):
            return [func(item) for item in collection]
        elif isinstance(collection, dict):
            return {key: func(value) for key, value in collection.items()}
        elif isinstance(collection, str):
            return ''.join(func(char) for char in collection)
        else:
            return func(collection)
    
    def time_exec(func, *args, **kwargs):
        """Time execution of function"""
        import time
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        return result, end - start
    
    def apply(func, *args, **kwargs):
        """Apply function with arguments"""
        return func(*args, **kwargs)
    
    def apply_iter(iterator, *args, **kwargs):
        """Apply functions from iterator with same arguments"""
        for func in iterator:
            yield func(*args, **kwargs)
    
    def papply(func, *args, **kwargs):
        """Partial application"""
        def partial(*more_args, **more_kwargs):
            return func(*args, *more_args, **kwargs, **more_kwargs)
        return partial
    

    
    def complement(pred):
        """Return complement of predicate"""
        def _complement(*args, **kwargs):
            return not pred(*args, **kwargs)
        return _complement
    
    def all_of(predicates):
        """Return predicate that is true if all predicates are true"""
        def _all_of(*args, **kwargs):
            for pred in predicates:
                if not pred(*args, **kwargs):
                    return False
            return True
        return _all_of
    
    def some_of(predicates):
        """Return predicate that is true if any predicate is true"""
        def _some_of(*args, **kwargs):
            for pred in predicates:
                if pred(*args, **kwargs):
                    return True
            return False
        return _some_of
    
    # Simple LRU cache implementation
    class LRUCache:
        def __init__(self, size=None):
            self.size = size
            self.cache = {}
            self.hits = 0
            self.misses = 0
        
        def get(self, key):
            if key in self.cache:
                self.hits += 1
                return self.cache[key]
            self.misses += 1
            return None
        
        def put(self, key, value):
            if self.size and len(self.cache) >= self.size:
                # Remove oldest item (simple implementation)
                oldest_key = next(iter(self.cache))
                del self.cache[oldest_key]
            self.cache[key] = value
    
    def cached(max_size=None):
        """Cache decorator"""
        def decorator(func):
            cache = LRUCache(max_size)
            def wrapper(*args, **kwargs):
                key = str(args) + str(sorted(kwargs.items()))
                result = cache.get(key)
                if result is None:
                    result = func(*args, **kwargs)
                    cache.put(key, result)
                return result
            return wrapper
        return decorator
    
    def clear_cache(wrapper):
        """Clear cache (no-op in fallback)"""
        pass
    
    def get_cache(wrapper):
        """Get cache (no-op in fallback)"""
        return None
    
    # Simple statistics implementations
    class Deviator:
        def __init__(self):
            self.count = 0
            self.sum = 0
            self.sum_sq = 0
        
        def add(self, value):
            self.count += 1
            self.sum += value
            self.sum_sq += value * value
        
        def mean(self):
            return self.sum / self.count if self.count > 0 else 0
        
        def variance(self):
            if self.count <= 1:
                return 0
            mean = self.mean()
            return (self.sum_sq / self.count) - (mean * mean)
        
        def std(self):
            import math
            return math.sqrt(self.variance())
    
    def empty_deviator():
        return Deviator()
    
    def apply_sequence(funcs, data):
        """Apply sequence of functions to data"""
        result = data
        for func in funcs:
            result = func(result)
        return result
    
    def zscore(value, mean, std):
        """Calculate z-score"""
        return (value - mean) / std if std != 0 else 0

# This class is a simple continuation class that allows for tail recursion
class _Continuation:
    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs

    def set_args(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs
    
    def _resolve_(self):
        return self.fn(*self.args, **self.kwargs)

_conts = [_Continuation(), _Continuation()]
_index = 0

class ContinuationPair():
    def __init__(self):
        self._conts = [_Continuation(), _Continuation()]
        self._index = 0
    
    def get_continuation(self):
        res = self._conts[self._index]
        self._index ^= 1
        return res

_allocated = _plist([])

def get_continuation():
    global _allocated
    return _allocated.first.get_continuation()

def enter_function():
    global _allocated
    _allocated = _allocated.cons(ContinuationPair())

def exit_function():
    global _allocated
    _allocated = _allocated.rest

def recurse(*args, **kwargs):
    """Function to be used instead of a recursive call in a tail recursive function"""
    recur = get_continuation()
    recur.args = args
    recur.kwargs = kwargs
    recur.fn = None
    return recur

def mutual(function, *args, **kwargs):
    """For mutual tail recursion"""
    recur = get_continuation()
    recur.args = args
    recur.kwargs = kwargs
    recur.fn = function
    return recur

def tail_recursive(f):
    """Decorator that allows tail recursion to occur"""
    def decorated(*args, **kwargs):
        enter_function()
        try:
            result = f(*args, **kwargs)
            while isinstance(result, _Continuation):
                if result.fn != None:
                    result = result.fn(*result.args, **result.kwargs)
                else:
                    result = f(*result.args, **result.kwargs)
            return result
        except:
            raise
        finally:
            exit_function()
    return decorated


def loop(bindings: List[Tuple[str, Any]], body_func):
    """
    Create a tail recursive loop using the adamantine tail recursion system.
    
    Args:
        bindings: List of (name, value) pairs for initial bindings
        body_func: Function that takes an environment dict and returns a value
    
    Example:
        (loop [i 0, sum 0]
          (if (>= i 10)
            sum
            (recur (+ i 1) (+ sum i))))
    """
    # Convert bindings to individual arguments
    binding_names = [name for name, _ in bindings]
    binding_values = [value for _, value in bindings]
    
    # Create a tail recursive function that takes individual arguments
    @tail_recursive
    def loop_func(*args):
        # Create environment from arguments
        env = dict(zip(binding_names, args))
        env['recur'] = recurse
        
        # Call the body function with the environment
        return body_func(env)
    
    # Start the loop with initial values
    return loop_func(*binding_values)


def recur(*args):
    """
    Perform tail recursion. Can be used in any function to make it tail recursive.
    In loop contexts, this calls the loop's recur function.
    In regular functions, this calls recurse() for general tail recursion.
    """
    # For now, just use recurse directly
    # TODO: Add proper loop context detection
    return recurse(*args)


def dot_access(obj, *members):
    """Access nested attributes: dot_access(obj, 'm1', 'm2') returns obj.m1.m2"""
    current = obj
    for member in members:
        current = getattr(current, member)
    return current


# ------------------ Lispish I/O helpers ------------------

def slurp(path: str, encoding: str = "utf-8") -> str:
    """Read entire file into a string (UTF-8 by default)."""
    with open(path, "r", encoding=encoding) as fh:
        return fh.read()

def spit(path: str, data: object, encoding: str = "utf-8", append: bool = False) -> None:
    """Write stringified data to file. If append=True, appends; otherwise overwrites."""
    mode = "a" if append else "w"
    with open(path, mode, encoding=encoding) as fh:
        fh.write(str(data))

def read_lines(path: str, encoding: str = "utf-8"):
    """Read file lines into a persistent vector (without trailing newlines)."""
    with open(path, "r", encoding=encoding) as fh:
        lines = [line.rstrip("\n") for line in fh]
    try:
        from pyrsistent import pvector as _pv
        return _pv(lines)
    except Exception:
        return lines

def write_lines(path: str, seq, encoding: str = "utf-8", newline: str = "\n", append: bool = False) -> None:
    """Write a sequence of lines to a file, adding newline between lines."""
    mode = "a" if append else "w"
    with open(path, mode, encoding=encoding) as fh:
        first = True
        for item in seq:
            if not first:
                fh.write(newline)
            fh.write(str(item))
            first = False

def with_open(path: str, mode: str, func, encoding: str = "utf-8"):
    """Open a file and call func(file), ensuring the file is closed. Returns func's result."""
    fh = open(path, mode, encoding=encoding)
    try:
        return func(fh)
    finally:
        try:
            fh.close()
        except Exception:
            pass

def read_stdin(encoding: str = "utf-8") -> str:
    """Read all of stdin as a string."""
    import sys
    data = sys.stdin.buffer.read()
    try:
        return data.decode(encoding)
    except Exception:
        return data.decode("utf-8", errors="replace")

def read_stdin_lines(encoding: str = "utf-8"):
    """Read stdin lines into a persistent vector (without trailing newlines)."""
    import sys
    lines = [line.rstrip("\n") for line in sys.stdin.read().splitlines()]
    try:
        from pyrsistent import pvector as _pv
        return _pv(lines)
    except Exception:
        return lines

def read(path: str, encoding: str = "utf-8") -> str:
    """Alias of slurp: read entire file as string."""
    return slurp(path, encoding)

def write(path: str, data: object, encoding: str = "utf-8") -> None:
    """Alias of spit (overwrite): write stringified data to file."""
    return spit(path, data, encoding=encoding, append=False)
