from __future__ import annotations
from typing import Any, List, Dict, Optional, Tuple, Set
from dataclasses import dataclass
from .lreader import Syn, Symbol, SrcSpan

class MacroError(Exception):
    pass

@dataclass
class Pattern:
    """Represents a pattern in a syntax-rules macro"""
    pattern: Syn
    literals: Set[str]
    
    def match(self, input_expr: Syn, bindings: Dict[str, Any]) -> bool:
        """Match input expression against this pattern, updating bindings"""
        return self._match_pattern(self.pattern, input_expr, bindings)
    
    def _match_pattern(self, pattern: Syn, input_expr: Syn, bindings: Dict[str, Any]) -> bool:
        """Recursively match pattern against input expression"""
        if pattern.tag == "symbol":
            symbol_name = pattern.val.qual
            if symbol_name in self.literals:
                # Literal symbol - must match exactly
                return (input_expr.tag == "symbol" and 
                       input_expr.val.qual == symbol_name)
            else:
                # Pattern variable - bind it
                bindings[symbol_name] = input_expr
                return True
        
        elif pattern.tag == "list":
            if input_expr.tag != "list":
                return False
            
            pattern_items = pattern.val
            input_items = input_expr.val
            
            # Handle ellipsis patterns
            if len(pattern_items) >= 2 and self._is_ellipsis(pattern_items[-1]):
                # Pattern ends with ellipsis
                if len(input_items) < len(pattern_items) - 2:
                    return False
                
                # Match fixed parts
                for i in range(len(pattern_items) - 2):
                    if not self._match_pattern(pattern_items[i], input_items[i], bindings):
                        return False
                
                # Match ellipsis part
                ellipsis_pattern = pattern_items[-2]
                ellipsis_var = self._get_ellipsis_var(ellipsis_pattern)
                if ellipsis_var:
                    # Bind the ellipsis variable to the remaining items
                    bindings[ellipsis_var] = input_items[len(pattern_items) - 2:]
                    return True
                # New: if the ellipsis pattern is a simple variable symbol, bind it to the list of remaining items
                if ellipsis_pattern.tag == "symbol":
                    var_name = ellipsis_pattern.val.qual
                    bindings[var_name] = input_items[len(pattern_items) - 2:]
                    return True
                # Otherwise, match each remaining item against the ellipsis pattern
                for i in range(len(pattern_items) - 2, len(input_items)):
                    if not self._match_pattern(ellipsis_pattern, input_items[i], bindings):
                        return False
                return True
            
            else:
                # No ellipsis - lengths must match
                if len(pattern_items) != len(input_items):
                    return False
                
                # Match each item
                for p_item, i_item in zip(pattern_items, input_items):
                    if not self._match_pattern(p_item, i_item, bindings):
                        return False
                return True
        
        elif pattern.tag == "vector":
            if input_expr.tag != "vector":
                return False
            
            pattern_items = pattern.val
            input_items = input_expr.val
            
            if len(pattern_items) != len(input_items):
                return False
            
            for p_item, i_item in zip(pattern_items, input_items):
                if not self._match_pattern(p_item, i_item, bindings):
                    return False
            return True
        
        else:
            # For other types, require exact match
            return (pattern.tag == input_expr.tag and 
                   pattern.val == input_expr.val)
    
    def _is_ellipsis(self, item: Syn) -> bool:
        """Check if item is an ellipsis"""
        return (item.tag == "symbol" and 
               item.val.qual == "...")
    
    def _get_ellipsis_var(self, pattern: Syn) -> Optional[str]:
        """Extract variable name from ellipsis pattern like (x ...)"""
        if (pattern.tag == "list" and 
            len(pattern.val) == 2 and
            pattern.val[0].tag == "symbol" and
            self._is_ellipsis(pattern.val[1])):
            return pattern.val[0].val.qual
        return None

@dataclass
class Template:
    """Represents a template in a syntax-rules macro"""
    template: Syn
    literals: Set[str]
    
    def expand(self, bindings: Dict[str, Any], gensym_counter: int) -> Syn:
        """Expand template using bindings, ensuring hygiene"""
        return self._expand_template(self.template, bindings, gensym_counter, {})
    
    def _expand_template(self, template: Syn, bindings: Dict[str, Any], 
                        gensym_counter: int, local_bindings: Dict[str, str]) -> Syn:
        """Recursively expand template"""
        if template.tag == "symbol":
            symbol_name = template.val.qual
            if symbol_name in self.literals:
                # Literal symbol - return as-is
                return template
            elif symbol_name in bindings:
                # Bound variable - return the bound value
                return bindings[symbol_name]
            else:
                # Unbound symbol - check if it's a special form, built-in, or ellipsis
                special_forms = {"if", "begin", "define", "lambda", "let", "set!", "loop", "quote", "quasiquote", "unquote", "splice"}
                built_ins = {"+", "-", "*", "/", "mod", "inc", "car", "cdr", "print", "map", "include", "take", "drop", "reverse", "sort", "list", "not", ".", "__getitem__", "cond", "let", "let*", "->", "->>"}
                
                if symbol_name in special_forms or symbol_name in built_ins or symbol_name == "...":
                    # Don't rename special forms, built-ins, or ellipsis
                    return template
                else:
                    # Unbound symbol - ensure hygiene by renaming if needed
                    if symbol_name in local_bindings:
                        # This symbol was introduced by the macro - keep it hygienic
                        new_name = local_bindings[symbol_name]
                    else:
                        # This is a free variable - should be hygienic
                        new_name = f"{symbol_name}__{gensym_counter}"
                        local_bindings[symbol_name] = new_name
                    
                    return Syn("symbol", Symbol(new_name), template.span)
        
        elif template.tag == "list":
            # Handle ellipsis expansion
            items = template.val
            expanded_items = []
            i = 0
            
            while i < len(items):
                if i < len(items) - 1 and self._is_ellipsis(items[i + 1]):
                    # Found ellipsis pattern: (var ...)
                    if items[i].tag == "symbol":
                        var_name = items[i].val.qual
                        if var_name in bindings:
                            # Expand ellipsis
                            ellipsis_values = bindings[var_name]
                            # ellipsis_values should be a list of Syn objects
                            if isinstance(ellipsis_values, list):
                                for value in ellipsis_values:
                                    expanded_items.append(value)
                            else:
                                # Single value
                                expanded_items.append(ellipsis_values)
                            i += 2  # Skip the variable and ellipsis
                        else:
                            # Variable not bound, treat as regular
                            expanded_items.append(
                                self._expand_template(items[i], bindings, gensym_counter, local_bindings)
                            )
                            i += 1
                    else:
                        # Not a simple variable, treat as regular
                        expanded_items.append(
                            self._expand_template(items[i], bindings, gensym_counter, local_bindings)
                        )
                        i += 1
                else:
                    # Regular item
                    expanded_items.append(
                        self._expand_template(items[i], bindings, gensym_counter, local_bindings)
                    )
                    i += 1
            
            return Syn("list", expanded_items, template.span)
        
        elif template.tag == "vector":
            expanded_items = [
                self._expand_template(item, bindings, gensym_counter, local_bindings)
                for item in template.val
            ]
            return Syn("vector", expanded_items, template.span)
        elif template.tag == "tuple":
            # Support ellipsis inside tuple templates similar to lists
            items = template.val
            expanded_items = []
            i = 0
            while i < len(items):
                if i < len(items) - 1 and self._is_ellipsis(items[i + 1]):
                    # Ellipsis expansion for tuple elements
                    if items[i].tag == "symbol":
                        var_name = items[i].val.qual
                        if var_name in bindings:
                            ellipsis_values = bindings[var_name]
                            if isinstance(ellipsis_values, list):
                                for value in ellipsis_values:
                                    expanded_items.append(value)
                            else:
                                expanded_items.append(ellipsis_values)
                            i += 2
                            continue
                    # Fallback regular expansion
                expanded_items.append(
                    self._expand_template(items[i], bindings, gensym_counter, local_bindings)
                )
                i += 1
            return Syn("tuple", expanded_items, template.span)
        
        else:
            # For other types, return as-is
            return template
    
    def _is_ellipsis(self, item: Syn) -> bool:
        """Check if item is an ellipsis"""
        return (item.tag == "symbol" and 
               item.val.qual == "...")
    
    def _get_ellipsis_var(self, pattern: Syn) -> Optional[str]:
        """Extract variable name from ellipsis pattern like (x ...)"""
        if (pattern.tag == "list" and 
            len(pattern.val) == 2 and
            pattern.val[0].tag == "symbol" and
            self._is_ellipsis(pattern.val[1])):
            return pattern.val[0].val.qual
        return None

@dataclass
class SyntaxRule:
    """Represents a single syntax rule in a macro"""
    pattern: Pattern
    template: Template
    
    def match_and_expand(self, input_expr: Syn, gensym_counter: int) -> Optional[Syn]:
        """Match input against pattern and expand template if successful"""
        bindings = {}
        if self.pattern.match(input_expr, bindings):
            return self.template.expand(bindings, gensym_counter)
        return None

@dataclass
class Macro:
    """Represents a hygienic macro"""
    name: str
    rules: List[SyntaxRule]
    literals: Set[str]
    gensym_counter: int = 0
    
    def expand(self, input_expr: Syn) -> Syn:
        """Expand input expression using this macro's rules"""
        for rule in self.rules:
            result = rule.match_and_expand(input_expr, self.gensym_counter)
            if result is not None:
                self.gensym_counter += 1
                return result
        
        raise MacroError(f"No matching rule found for macro {self.name}")

class MacroExpander:
    """Handles macro expansion during compilation"""
    
    def __init__(self):
        self.macros: Dict[str, Macro] = {}
        self.gensym_counter = 0
    
    def define_macro(self, name: str, literals: List[str], rules: List[Tuple[Syn, Syn]]) -> None:
        """Define a new macro"""
        literal_set = set(literals)
        syntax_rules = []
        
        for pattern, template in rules:
            pattern_obj = Pattern(pattern, literal_set)
            template_obj = Template(template, literal_set)
            syntax_rules.append(SyntaxRule(pattern_obj, template_obj))
        
        self.macros[name] = Macro(name, syntax_rules, literal_set, self.gensym_counter)
    
    def expand_macros(self, expr: Syn) -> Syn:
        """Expand all macros in an expression"""
        if expr.tag == "list":
            items = expr.val
            if not items:
                return expr
            
            head = items[0]
            if head.tag == "symbol" and head.val.qual in self.macros:
                # This is a macro call
                macro = self.macros[head.val.qual]
                expanded = macro.expand(expr)
                # Recursively expand the result to a fixed point
                return self.expand_macros(expanded)
            else:
                # Recursively expand sub-expressions
                expanded_items = [self.expand_macros(item) for item in items]
                return Syn("list", expanded_items, expr.span)
        
        elif expr.tag == "vector":
            expanded_items = [self.expand_macros(item) for item in expr.val]
            return Syn("vector", expanded_items, expr.span)
        
        elif expr.tag == "quote":
            # Don't expand inside quotes
            return expr
        
        else:
            return expr

def parse_syntax_rules(syntax_rules_expr: Syn) -> Tuple[List[str], List[Tuple[Syn, Syn]]]:
    """Parse a syntax-rules expression into literals and rules"""
    if syntax_rules_expr.tag != "list":
        raise MacroError("syntax-rules must be a list")
    
    items = syntax_rules_expr.val
    if len(items) < 3:
        raise MacroError("syntax-rules requires at least literals and one rule")
    
    # Parse literals
    literals_expr = items[1]
    if literals_expr.tag != "list":
        raise MacroError("syntax-rules literals must be a list")
    
    literals = []
    for literal in literals_expr.val:
        if literal.tag != "symbol":
            raise MacroError("syntax-rules literals must be symbols")
        literals.append(literal.val.qual)
    
    # Parse rules
    rules = []
    for rule_expr in items[2:]:
        if rule_expr.tag != "list" or len(rule_expr.val) != 2:
            raise MacroError("syntax-rules rule must be (pattern template)")
        
        pattern, template = rule_expr.val
        rules.append((pattern, template))
    
    return literals, rules

# Global macro expander
macro_expander = MacroExpander()

def define_syntax_rules(name: str, literals: List[str], rules: List[Tuple[Syn, Syn]]) -> None:
    """Define a syntax-rules macro"""
    macro_expander.define_macro(name, literals, rules)

def expand_macros(expr: Syn) -> Syn:
    """Expand all macros in an expression"""
    return macro_expander.expand_macros(expr)

# --- Standard macros installed by default ---

def _install_standard_macros() -> None:
    # index macro: recursive expansion for nested indexing
    from .lreader import _sym
    span = SrcSpan("<macro>",1,1,1,1)
    dot_sym = Syn("symbol", _sym("."), span)
    getitem_sym = Syn("symbol", _sym("__getitem__"), span)

    # Rule 1: (index coll idx) -> ((. coll __getitem__) idx)
    pat1 = Syn("list", [Syn("symbol", _sym("index"), span),
                         Syn("symbol", _sym("coll"), span),
                         Syn("symbol", _sym("idx"), span)], span)
    tmpl1 = Syn("list", [
        Syn("list", [dot_sym, Syn("symbol", _sym("coll"), span), getitem_sym], span),
        Syn("symbol", _sym("idx"), span)
    ], span)

    # Rule 2: (index coll idx1 idxrest ...) -> (index ((. coll __getitem__) idx1) idxrest ...)
    pat2 = Syn("list", [Syn("symbol", _sym("index"), span),
                         Syn("symbol", _sym("coll"), span),
                         Syn("symbol", _sym("idx1"), span),
                         Syn("symbol", _sym("idxrest"), span),
                         Syn("symbol", _sym("..."), span)], span)
    tmpl2 = Syn("list", [
        Syn("symbol", _sym("index"), span),
        Syn("list", [dot_sym, Syn("symbol", _sym("coll"), span), getitem_sym], span),
        Syn("symbol", _sym("idx1"), span),
        Syn("symbol", _sym("idxrest"), span),
        Syn("symbol", _sym("..."), span)
    ], span)

    define_syntax_rules("index", [], [(pat1, tmpl1), (pat2, tmpl2)])

    # let*: sequential bindings
    from .lreader import _sym
    span = SrcSpan("<macro>",1,1,1,1)
    let_sym = Syn("symbol", _sym("let"), span)
    begin_sym = Syn("symbol", _sym("begin"), span)
    letstar_sym = Syn("symbol", _sym("let*"), span)

    #
    # (let* () body ...) -> (begin body ...)
    pat_ls0 = Syn("list", [letstar_sym,
                             Syn("list", [], span),
                             Syn("symbol", _sym("body"), span),
                             Syn("symbol", _sym("..."), span)], span)
    tmpl_ls0 = Syn("list", [begin_sym,
                              Syn("symbol", _sym("body"), span),
                              Syn("symbol", _sym("..."), span)], span)

    # (let* ((n v) rest ...) body ...) -> (let ((n v)) (let* (rest ...) body ...))
    pat_ls1 = Syn("list", [letstar_sym,
                             Syn("list", [Syn("list", [Syn("symbol", _sym("n"), span), Syn("symbol", _sym("v"), span)], span),
                                            Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span),
                             Syn("symbol", _sym("body"), span), Syn("symbol", _sym("..."), span)], span)
    tmpl_ls1 = Syn("list", [let_sym,
                              Syn("list", [Syn("list", [Syn("symbol", _sym("n"), span), Syn("symbol", _sym("v"), span)], span)], span),
                              Syn("list", [letstar_sym,
                                            Syn("list", [Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span),
                                            Syn("symbol", _sym("body"), span), Syn("symbol", _sym("..."), span)], span)], span)

    define_syntax_rules("let*", [], [(pat_ls0, tmpl_ls0), (pat_ls1, tmpl_ls1)])

    # cond macro
    cond_sym = Syn("symbol", _sym("cond"), span)
    if_sym = Syn("symbol", _sym("if"), span)
    else_sym = Syn("symbol", _sym("else"), span)

    # (cond (else body ...)) -> (begin body ...)
    pat_c0 = Syn("list", [cond_sym,
                            Syn("list", [else_sym, Syn("symbol", _sym("body"), span), Syn("symbol", _sym("..."), span)], span)], span)
    tmpl_c0 = Syn("list", [begin_sym, Syn("symbol", _sym("body"), span), Syn("symbol", _sym("..."), span)], span)

    # (cond (test body ...) rest ...) -> (if test (begin body ...) (cond rest ...))
    pat_c1 = Syn("list", [cond_sym,
                            Syn("list", [Syn("symbol", _sym("test"), span), Syn("symbol", _sym("body"), span), Syn("symbol", _sym("..."), span)], span),
                            Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span)
    tmpl_c1 = Syn("list", [if_sym,
                             Syn("symbol", _sym("test"), span),
                             Syn("list", [begin_sym, Syn("symbol", _sym("body"), span), Syn("symbol", _sym("..."), span)], span),
                             Syn("list", [cond_sym, Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span)], span)

    define_syntax_rules("cond", ["else"], [(pat_c0, tmpl_c0), (pat_c1, tmpl_c1)])

    # Threading macros -> and ->>
    thread_first = Syn("symbol", _sym("->"), span)
    thread_last = Syn("symbol", _sym("->>"), span)

    # (-> x forms ...) => fold: insert x as first arg into each list form, sequentially
    # Implement via small-step recursion with base rule
    pat_t0 = Syn("list", [thread_first, Syn("symbol", _sym("x"), span)], span)
    tmpl_t0 = Syn("symbol", _sym("x"), span)
    # (-> x (f args ...) rest ...) -> (-> (f x args ...) rest ...)
    pat_t1 = Syn("list", [thread_first,
                            Syn("symbol", _sym("x"), span),
                            Syn("list", [Syn("symbol", _sym("f"), span), Syn("symbol", _sym("args"), span), Syn("symbol", _sym("..."), span)], span),
                            Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span)
    tmpl_t1 = Syn("list", [thread_first,
                             Syn("list", [Syn("symbol", _sym("f"), span), Syn("symbol", _sym("x"), span), Syn("symbol", _sym("args"), span), Syn("symbol", _sym("..."), span)], span),
                             Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span)

    define_syntax_rules("->", [], [(pat_t0, tmpl_t0), (pat_t1, tmpl_t1)])

    # (->> x) -> x
    pat_tf0 = Syn("list", [thread_last, Syn("symbol", _sym("x"), span)], span)
    tmpl_tf0 = Syn("symbol", _sym("x"), span)
    # (->> x (op args ...) rest ...) -> (->> (op args ... x) rest ...)
    pat_tf1 = Syn("list", [thread_last,
                             Syn("symbol", _sym("x"), span),
                             Syn("list", [Syn("symbol", _sym("op"), span), Syn("symbol", _sym("args"), span), Syn("symbol", _sym("..."), span)], span),
                             Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span)
    tmpl_tf1 = Syn("list", [thread_last,
                              Syn("list", [Syn("symbol", _sym("op"), span), Syn("symbol", _sym("args"), span), Syn("symbol", _sym("..."), span), Syn("symbol", _sym("x"), span)], span),
                              Syn("symbol", _sym("rest"), span), Syn("symbol", _sym("..."), span)], span)
    define_syntax_rules("->>", [], [(pat_tf0, tmpl_tf0), (pat_tf1, tmpl_tf1)])

_install_standard_macros()
