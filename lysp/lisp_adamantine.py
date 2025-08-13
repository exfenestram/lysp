from __future__ import annotations
from typing import Any, List, Tuple, Dict, Callable, Iterator, Iterable
from .lreader import Syn, SrcSpan

# Import adamantine iterator helpers directly to avoid circular import
try:
    from adamantine.exec_models import map_iter, cmap_iter, emap_iter
    from adamantine.predicates import include, exclude, split
    from adamantine.exec_models import foldl as _foldl_impl
    from adamantine.partial import papply as _papply_impl
except ImportError:
    # Fallback implementations
    def map_iter(f, *args):
        if not args:
            return
        if len(args) == 1:
            for item in args[0]:
                yield f(item)
        else:
            for item in zip(*args):
                yield f(*item)
    
    def cmap_iter(f, *args):
        if not args:
            return
        if len(args) == 1:
            for item in args[0]:
                try:
                    yield f(item)
                except Exception:
                    pass
        else:
            for item in zip(*args):
                try:
                    yield f(*item)
                except Exception:
                    pass
    
    def emap_iter(f, *args):
        if not args:
            return
        if len(args) == 1:
            for item in args[0]:
                try:
                    yield f(item)
                except Exception as e:
                    yield e
        else:
            for item in zip(*args):
                try:
                    yield f(*item)
                except Exception as e:
                    yield e
    
    def include(pred, iterator):
        for el in iterator:
            if pred(el):
                yield el
    
    def exclude(pred, iterator):
        for el in iterator:
            if not pred(el):
                yield el
    
    def split(pred, iterator):
        true_items = []
        false_items = []
        for el in iterator:
            if pred(el):
                true_items.append(el)
            else:
                false_items.append(el)
        return iter(true_items), iter(false_items)
    
    def _foldl_impl(f, initializer, iterable):
        acc = initializer
        for x in iterable:
            acc = f(acc, x)
        return acc
    
    def _papply_impl(func, *args, **kwargs):
        def partial(*more_args, **more_kwargs):
            return func(*args, *more_args, **kwargs, **more_kwargs)
        return partial

def _get_lisp_type_and_items(data: Any) -> Tuple[str, List[Any], SrcSpan]:
    """Extract type, items, and span from Lisp data structure"""



    if isinstance(data, Syn):
        if data.tag in ("list", "vector", "set", "tuple"):
            return data.tag, data.val, data.span
        elif data.tag == "map":
            return data.tag, data.val, data.span
        else:
            # For other types, treat as list
            return "list", [data], data.span
    elif isinstance(data, list):
        # For Python lists, treat as list
        return "list", list(data), SrcSpan("<python>", 1, 1, 1, 1)
    elif isinstance(data, tuple):
        # For Python tuples, treat as tuple
        return "tuple", list(data), SrcSpan("<python>", 1, 1, 1, 1)
    else:
        # Check if it's a pyrsistent type
        try:
            # Check if it's a pyrsistent map
            if hasattr(data, 'iterkeys') and hasattr(data, 'itervalues'):
                # It's a map-like object
                return "map", list(data.items()), SrcSpan("<python>", 1, 1, 1, 1)
            elif hasattr(data, 'add') and hasattr(data, 'remove'):
                # It's a set-like object
                return "set", list(data), SrcSpan("<python>", 1, 1, 1, 1)
            elif hasattr(data, 'cons') and hasattr(data, 'first'):
                # It's a list-like object (plist)
                items = list(data)
                return "list", items, SrcSpan("<python>", 1, 1, 1, 1)
            elif hasattr(data, 'append') or hasattr(data, 'extend'):
                # It's a vector-like object (pvector)
                items = list(data)
                return "vector", items, SrcSpan("<python>", 1, 1, 1, 1)
            else:
                # Try to iterate over it to see if it's a sequence
                items = list(data)
                return "vector", items, SrcSpan("<python>", 1, 1, 1, 1)
        except (TypeError, AttributeError):
            # For other types, treat as list
            return "list", [data], SrcSpan("<python>", 1, 1, 1, 1)

def _create_lisp_result(data_type: str, items: List[Any], span: SrcSpan) -> Any:
    """Create Lisp data structure of the specified type"""
    try:
        from pyrsistent import pvector, pset, pmap, plist
        if data_type == "list":
            return plist(items)
        elif data_type == "vector":
            return pvector(items)
        elif data_type == "set":
            return pset(items)
        elif data_type == "map":
            # For maps, items should be key-value pairs
            # If items are single values, we need to handle this differently
            try:
                return pmap(dict(items))
            except (ValueError, TypeError):
                # If items are not key-value pairs, create a new map with indices as keys
                return pmap({i: item for i, item in enumerate(items)})
        elif data_type == "tuple":
            return tuple(items)
        else:
            return plist(items)
    except ImportError:
        # Fallback without pyrsistent
        if data_type == "list":
            return list(items)
        elif data_type == "vector":
            return list(items)
        elif data_type == "set":
            return set(items)
        elif data_type == "map":
            # For maps, items should be key-value pairs
            # If items are single values, we need to handle this differently
            try:
                return dict(items)
            except (ValueError, TypeError):
                # If items are not key-value pairs, create a new map with indices as keys
                return {i: item for i, item in enumerate(items)}
        elif data_type == "tuple":
            return tuple(items)
        else:
            return list(items)

def lisp_map(f: Callable, *args) -> Any:
    """Map function over Lisp data structures, preserving type"""
    if not args:
        return Syn("list", [], SrcSpan("<empty>", 1, 1, 1, 1))
    
    # Get the first argument to determine the output type
    first_arg = args[0]
    data_type, _, span = _get_lisp_type_and_items(first_arg)
    
    # Extract items from all arguments
    arg_items = []
    for arg in args:
        _, items, _ = _get_lisp_type_and_items(arg)
        arg_items.append(items)
    

    
    # Apply the function using the iterator version
    if len(arg_items) == 1:
        result_items = list(map_iter(f, arg_items[0]))
    else:
        result_items = list(map_iter(f, *arg_items))
    
    return _create_lisp_result(data_type, result_items, span)

def lisp_cmap(f: Callable, *args) -> Any:
    """Map function over Lisp data structures, catching exceptions, preserving type"""
    if not args:
        return Syn("list", [], SrcSpan("<empty>", 1, 1, 1, 1))
    
    # Get the first argument to determine the output type
    first_arg = args[0]
    data_type, _, span = _get_lisp_type_and_items(first_arg)
    
    # Extract items from all arguments
    arg_items = []
    for arg in args:
        _, items, _ = _get_lisp_type_and_items(arg)
        arg_items.append(items)
    
    # Apply the function using the iterator version
    if len(arg_items) == 1:
        result_items = list(cmap_iter(f, arg_items[0]))
    else:
        result_items = list(cmap_iter(f, *arg_items))
    
    return _create_lisp_result(data_type, result_items, span)

def lisp_emap(f: Callable, *args) -> Any:
    """Map function over Lisp data structures, returning exceptions, preserving type"""
    if not args:
        return Syn("list", [], SrcSpan("<empty>", 1, 1, 1, 1))
    
    # Get the first argument to determine the output type
    first_arg = args[0]
    data_type, _, span = _get_lisp_type_and_items(first_arg)
    
    # Extract items from all arguments
    arg_items = []
    for arg in args:
        _, items, _ = _get_lisp_type_and_items(arg)
        arg_items.append(items)
    
    # Apply the function using the iterator version
    if len(arg_items) == 1:
        result_items = list(emap_iter(f, arg_items[0]))
    else:
        result_items = list(emap_iter(f, *arg_items))
    
    return _create_lisp_result(data_type, result_items, span)

def lisp_include(pred: Callable, data: Any) -> Any:
    """Include elements that satisfy predicate, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    result_items = list(include(pred, items))
    return _create_lisp_result(data_type, result_items, span)

def lisp_exclude(pred: Callable, data: Any) -> Any:
    """Exclude elements that satisfy predicate, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    result_items = list(exclude(pred, items))
    return _create_lisp_result(data_type, result_items, span)

def lisp_split(pred: Callable, data: Any) -> Tuple[Any, Any]:
    """Split data based on predicate, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    true_iter, false_iter = split(pred, items)
    true_items = list(true_iter)
    false_items = list(false_iter)
    
    true_result = _create_lisp_result(data_type, true_items, span)
    false_result = _create_lisp_result(data_type, false_items, span)
    
    return true_result, false_result

def lisp_filter(pred: Callable, data: Any) -> Any:
    """Alias for lisp_include for consistency"""
    return lisp_include(pred, data)

def lisp_remove(pred: Callable, data: Any) -> Any:
    """Alias for lisp_exclude for consistency"""
    return lisp_exclude(pred, data)

# Additional utility functions
def lisp_concat(*args) -> Any:
    """Concatenate Lisp data structures, preserving type of first argument"""
    if not args:
        return Syn("list", [], SrcSpan("<empty>", 1, 1, 1, 1))
    
    first_arg = args[0]
    data_type, _, span = _get_lisp_type_and_items(first_arg)
    
    all_items = []
    for arg in args:
        _, items, _ = _get_lisp_type_and_items(arg)
        all_items.extend(items)
    
    return _create_lisp_result(data_type, all_items, span)

def lisp_take(n: int, data: Any) -> Any:
    """Take first n elements, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    result_items = items[:n]
    return _create_lisp_result(data_type, result_items, span)

def lisp_drop(n: int, data: Any) -> Any:
    """Drop first n elements, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    result_items = items[n:]
    return _create_lisp_result(data_type, result_items, span)

def lisp_reverse(data: Any) -> Any:
    """Reverse elements, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    result_items = list(reversed(items))
    return _create_lisp_result(data_type, result_items, span)

def lisp_sort(data: Any, key: Callable = None, reverse: bool = False) -> Any:
    """Sort elements, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    result_items = sorted(items, key=key, reverse=reverse)
    return _create_lisp_result(data_type, result_items, span)

def lisp_unique(data: Any) -> Any:
    """Remove duplicates, preserving type"""
    data_type, items, span = _get_lisp_type_and_items(data)
    seen = set()
    result_items = []
    for item in items:
        if item not in seen:
            seen.add(item)
            result_items.append(item)
    return _create_lisp_result(data_type, result_items, span)

# --- Additional helpers exposed in Lisp ---

def lisp_foldl(f: Callable[[Any, Any], Any], initializer: Any, data: Any) -> Any:
    """Left fold over a collection, producing a scalar accumulator."""
    _, items, _ = _get_lisp_type_and_items(data)
    return _foldl_impl(f, initializer, items)

def lisp_group_by(data: Any, key: Callable[[Any], Any] = lambda x: x, value: Callable[[Any], Any] = lambda x: x) -> Dict[Any, List[Any]]:
    """Group items by key function. Returns a Python dict of key -> list(values)."""
    _, items, _ = _get_lisp_type_and_items(data)
    groups: Dict[Any, List[Any]] = {}
    for item in items:
        k = key(item)
        groups.setdefault(k, []).append(value(item))
    return groups

def lisp_papply(func: Callable, *args, **kwargs) -> Callable:
    """Partial application; returns a function with preset positional/keyword args."""
    return _papply_impl(func, *args, **kwargs)
