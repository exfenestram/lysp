from __future__ import annotations
import io
from dataclasses import dataclass
from fractions import Fraction
from typing import Any, List, Tuple, Dict, Optional

__all__ = [
    "SrcSpan", "Syn", "Symbol", "Keyword",
    "Reader", "ReaderError", "_sym", "_kw"
]

# Intern pools for symbols/keywords
_symbol_pool: Dict[str, "Symbol"] = {}
_keyword_pool: Dict[str, "Keyword"] = {}

def _sym(qual: str) -> "Symbol":
    s = _symbol_pool.get(qual)
    if s is None:
        s = Symbol(qual)
        _symbol_pool[qual] = s
    return s

def _kw(qual: str) -> "Keyword":
    k = _keyword_pool.get(qual)
    if k is None:
        k = Keyword(qual)
        _keyword_pool[qual] = k
    return k

# ----------------------------
# Syntax objects
# ----------------------------
@dataclass(frozen=True)
class SrcSpan:
    file: str
    start_line: int
    start_col: int
    end_line: int
    end_col: int

@dataclass(frozen=True)
class Syn:
    tag: str
    val: Any
    span: SrcSpan

    def with_val(self, new_val: Any) -> "Syn":
        return Syn(self.tag, new_val, self.span)

# Atoms
@dataclass(frozen=True)
class Symbol:
    qual: str  # e.g., "ns/name" or just "name"
    def __repr__(self) -> str:
        return f"'{self.qual}"

@dataclass(frozen=True)
class Keyword:
    qual: str  # e.g., "ns/name"
    def __repr__(self) -> str:
        return f":{self.qual}"

# ----------------------------
# Reader / Lexer
# ----------------------------
class ReaderError(Exception):
    pass

WHITESPACE = set(" 	,\n\r")  # commas are whitespace

@dataclass
class Reader:
    src: str
    file: str = "<stdin>"
    i: int = 0
    line: int = 1
    col: int = 0

    def eof(self) -> bool:
        return self.i >= len(self.src)

    def peek(self, n: int = 0) -> str:
        j = self.i + n
        return "" if j >= len(self.src) else self.src[j]

    def advance(self, n: int = 1):
        for _ in range(n):
            if self.eof():
                return
            ch = self.src[self.i]
            self.i += 1
            if ch == "\n":
                self.line += 1
                self.col = 0
            else:
                self.col += 1

    def span_from(self, sl: int, sc: int) -> SrcSpan:
        return SrcSpan(self.file, sl, sc, self.line, self.col)

    def skip_ws_comments(self):
        while True:
            while not self.eof() and self.peek() in WHITESPACE:
                self.advance()
            if self.peek() == ";":  # line comment
                while not self.eof() and self.peek() != "\n":
                    self.advance()
                continue
            if self.peek() == "#" and self.peek(1) == "|":  # block comment
                self.advance(2)
                depth = 1
                while depth > 0 and not self.eof():
                    if self.peek() == "#" and self.peek(1) == "|":
                        depth += 1; self.advance(2)
                    elif self.peek() == "|" and self.peek(1) == "#":
                        depth -= 1; self.advance(2)
                    else:
                        self.advance()
                continue
            break

    def read(self) -> List[Syn]:
        forms: List[Syn] = []
        self.skip_ws_comments()
        while not self.eof():
            forms.append(self.read_form())
            self.skip_ws_comments()
        return forms

    def read_form(self) -> Syn:
        self.skip_ws_comments()
        sl, sc = self.line, self.col
        ch = self.peek()
        if ch == "":
            raise ReaderError("unexpected EOF")
        if ch == "(":
            return self.read_list(sl, sc)
        if ch == "[":
            return self.read_vector(sl, sc)
        if ch == "{" and self.peek(1) != "#":
            return self.read_map(sl, sc)
        if ch == "#" and self.peek(1) == "{":
            return self.read_set(sl, sc)
        if ch == "#" and self.peek(1) == "(":
            return self.read_tuple(sl, sc)
        if ch == '"':
            return self.read_string(sl, sc)
        if ch == "'":
            self.advance(); form = self.read_form()
            return Syn("quote", form, self.span_from(sl, sc))
        if ch == ":":
            return self.read_keyword(sl, sc)
        if ch == "`":
            self.advance(); form = self.read_form()
            return Syn("quasiquote", form, self.span_from(sl, sc))
        if ch == ",":
            self.advance(); tag = "splice" if self.peek() == "@" else "unquote"
            if tag == "splice": self.advance()
            form = self.read_form()
            return Syn(tag, form, self.span_from(sl, sc))

        if ch == "#" and self.peek(1) == '"':
            return self.read_regex(sl, sc)
        if ch == ".":
            return self.read_dot(sl, sc)
        if ch in "+-0123456789":
            num = self.try_number()
            if num is not None:
                return Syn("number", num, self.span_from(sl, sc))
        return self.read_symbol_or_special(sl, sc)

    def read_list(self, sl: int, sc: int) -> Syn:
        self.advance()
        items: List[Syn] = []
        self.skip_ws_comments()
        while self.peek() != ")":
            if self.eof(): raise ReaderError("unterminated list")
            items.append(self.read_form()); self.skip_ws_comments()
        self.advance()
        
        # Check if this is an import or export form
        if items and items[0].tag == "symbol":
            first_symbol = items[0].val.qual
            if first_symbol == "import":
                return self._parse_import_form(items, sl, sc)
            elif first_symbol == "export":
                return self._parse_export_form(items, sl, sc)
        
        return Syn("list", items, self.span_from(sl, sc))

    def read_vector(self, sl: int, sc: int) -> Syn:
        self.advance()
        items: List[Syn] = []
        self.skip_ws_comments()
        while self.peek() != "]":
            if self.eof(): raise ReaderError("unterminated vector")
            items.append(self.read_form()); self.skip_ws_comments()
        self.advance()
        return Syn("vector", items, self.span_from(sl, sc))

    def read_map(self, sl: int, sc: int) -> Syn:
        self.advance()
        items: List[Tuple[Syn, Syn]] = []
        self.skip_ws_comments()
        while self.peek() != "}":
            if self.eof(): raise ReaderError("unterminated map")
            k = self.read_form(); self.skip_ws_comments()
            v = self.read_form(); items.append((k, v)); self.skip_ws_comments()
        self.advance()
        return Syn("map", items, self.span_from(sl, sc))

    def read_set(self, sl: int, sc: int) -> Syn:
        self.advance(2)
        items: List[Syn] = []
        self.skip_ws_comments()
        while self.peek() != "}":
            if self.eof(): raise ReaderError("unterminated set")
            items.append(self.read_form()); self.skip_ws_comments()
        self.advance()
        return Syn("set", items, self.span_from(sl, sc))

    def read_tuple(self, sl: int, sc: int) -> Syn:
        self.advance(2)
        items: List[Syn] = []
        self.skip_ws_comments()
        while self.peek() != ")":
            if self.eof(): raise ReaderError("unterminated tuple")
            items.append(self.read_form()); self.skip_ws_comments()
        self.advance()
        return Syn("tuple", items, self.span_from(sl, sc))

    def read_string(self, sl: int, sc: int) -> Syn:
        assert self.peek() == '"'; self.advance()
        out = []
        while True:
            if self.eof(): raise ReaderError("unterminated string")
            ch = self.peek()
            if ch == '"': self.advance(); break
            if ch == "\\":
                self.advance(); esc = self.peek(); self.advance()
                mapping = {'"': '"', "\\": "\\", "/": "/", "b": "", "n": "\n", "r": "\r", "t": "\t"}
                if esc in mapping: out.append(mapping[esc])
                elif esc == "u":
                    hexs = self.src[self.i:self.i+4]
                    if len(hexs) < 4: raise ReaderError("bad unicode escape")
                    self.advance(4); out.append(chr(int(hexs, 16)))
                else: raise ReaderError(f"bad escape: \\{esc}")
            else:
                out.append(ch); self.advance()
        return Syn("string", ''.join(out), self.span_from(sl, sc))

    def read_keyword(self, sl: int, sc: int) -> Syn:
        assert self.peek() == ":"; self.advance()
        name = self.read_symbol_name()
        return Syn("keyword", _kw(name), self.span_from(sl, sc))

    def _parse_import_form(self, items: List[Syn], sl: int, sc: int) -> Syn:
        """Parse (import ...) form"""
        if len(items) < 2:
            raise ReaderError("import requires at least a module name")
        
        module_item = items[1]
        if module_item.tag not in ("symbol", "string"):
            raise ReaderError("module name must be a symbol or string")
        
        module_name = module_item.val if module_item.tag == "string" else module_item.val.qual
        
        # Parse options
        module_alias = None
        entities = None
        entity_aliases = None
        import_all = False
        
        i = 2
        while i < len(items):
            item = items[i]
            
            if item.tag == "keyword" and item.val.qual == "as":
                # Module alias
                if i + 1 >= len(items):
                    raise ReaderError(":as requires an alias name")
                alias_item = items[i + 1]
                if alias_item.tag != "symbol":
                    raise ReaderError("alias must be a symbol")
                module_alias = alias_item.val.qual
                i += 2
                
            elif item.tag == "keyword" and item.val.qual == "all":
                import_all = True
                i += 1
                
            elif item.tag == "list":
                # Entity list
                entities = []
                entity_aliases = {}
                
                for entity_item in item.val:
                    if entity_item.tag in ("symbol", "string"):
                        entity_name = entity_item.val if entity_item.tag == "string" else entity_item.val.qual
                        entities.append(entity_name)
                    else:
                        raise ReaderError("entity must be a symbol or string")
                
                i += 1
                
            else:
                # For now, just skip unexpected items
                i += 1
        
        # :all takes precedence over specific entities
        if import_all:
            return Syn("import-module", {
                "module": module_name,
                "alias": module_alias,
                "all": True
            }, self.span_from(sl, sc))
        
        if entities:
            return Syn("import-from", {
                "module": module_name,
                "module_alias": module_alias,
                "entities": entities,
                "entity_aliases": entity_aliases
            }, self.span_from(sl, sc))
        else:
            return Syn("import-module", {
                "module": module_name,
                "alias": module_alias
            }, self.span_from(sl, sc))

    def _parse_export_form(self, items: List[Syn], sl: int, sc: int) -> Syn:
        """Parse (export ...) form"""
        if len(items) < 2:
            raise ReaderError("export requires at least a name")
        
        name_item = items[1]
        if name_item.tag != "symbol":
            raise ReaderError("export name must be a symbol")
        name = name_item.val.qual
        
        # Parse options
        python_name = None
        module_name = "lisp"  # Default
        
        i = 2
        while i < len(items):
            item = items[i]
            
            if item.tag == "keyword" and item.val.qual == "as":
                # Python name alias
                if i + 1 >= len(items):
                    raise ReaderError(":as requires a name")
                alias_item = items[i + 1]
                if alias_item.tag != "symbol":
                    raise ReaderError("alias must be a symbol")
                python_name = alias_item.val.qual
                i += 2
                
            elif item.tag == "keyword" and item.val.qual == "to":
                # Module name
                if i + 1 >= len(items):
                    raise ReaderError(":to requires a module name")
                module_item = items[i + 1]
                if module_item.tag != "symbol":
                    raise ReaderError("module name must be a symbol")
                module_name = module_item.val.qual
                i += 2
                
            else:
                # For now, just skip unexpected items
                i += 1
        
        return Syn("export", {
            "name": name,
            "python_name": python_name,
            "module_name": module_name
        }, self.span_from(sl, sc))

    def read_dot(self, sl: int, sc: int) -> Syn:
        """Read dot operator: (. object m1 m2 ...) returns object.m1.m2..."""
        assert self.peek() == "."
        self.advance()  # Skip "."
        self.skip_ws_comments()
        
        # Read the object
        obj = self.read_form()
        self.skip_ws_comments()
        
        # Read the member names
        members = []
        while not self.eof() and self.peek() not in "()[]{}#\"'`,":
            member = self.read_symbol_or_special(sl, sc)
            if member.tag != "symbol":
                raise ReaderError("member name must be a symbol")
            members.append(member.val.qual)
            self.skip_ws_comments()
        
        return Syn("dot", {"object": obj, "members": members}, self.span_from(sl, sc))

    def read_regex(self, sl: int, sc: int) -> Syn:
        assert self.peek() == "#" and self.peek(1) == '"'; self.advance(2)
        out = []
        while True:
            if self.eof(): raise ReaderError("unterminated regex")
            ch = self.peek()
            if ch == '"': self.advance(); break
            if ch == "\\": self.advance(); out.append("\\" + self.peek()); self.advance()
            else: out.append(ch); self.advance()
        return Syn("regex", ''.join(out), self.span_from(sl, sc))

    def try_number(self) -> Optional[Any]:
        j = self.i; had_digit = False; is_float = False
        if self.peek() in "+-": j += 1
        while j < len(self.src) and self.src[j].isdigit(): had_digit = True; j += 1
        if j < len(self.src) and self.src[j] == "/":
            j += 1; dstart = j
            while j < len(self.src) and self.src[j].isdigit(): j += 1
            if dstart == j: return None
            text = self.src[self.i:j]; self.i = j
            num_s, den_s = text.split("/")
            return Fraction(int(num_s), int(den_s))
        if j < len(self.src) and self.src[j] == ".": is_float = True; j += 1
        while j < len(self.src) and self.src[j].isdigit(): j += 1
        if j < len(self.src) and self.src[j] in "eE":
            is_float = True; j += 1
            if j < len(self.src) and self.src[j] in "+-": j += 1
            ds = 0
            while j < len(self.src) and self.src[j].isdigit(): j += 1; ds += 1
            if ds == 0: return None
        if not had_digit and not is_float: return None
        text = self.src[self.i:j]; self.i = j
        return float(text) if is_float else int(text)

    def read_symbol_or_special(self, sl: int, sc: int) -> Syn:
        name = self.read_symbol_name()
        if name in ("true", "#t"): return Syn("boolean", True, self.span_from(sl, sc))
        if name in ("false", "#f"): return Syn("boolean", False, self.span_from(sl, sc))
        if name == "nil": return Syn("nil", None, self.span_from(sl, sc))
        return Syn("symbol", _sym(name), self.span_from(sl, sc))

    def read_symbol_name(self) -> str:
        if self.eof(): raise ReaderError("unexpected EOF reading symbol")
        name = []
        ch = self.peek()
        while ch and ch not in WHITESPACE and ch not in "()[]{}" and ch != '"':
            name.append(ch); self.advance(); ch = self.peek()
        return ''.join(name)