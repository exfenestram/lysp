from __future__ import annotations
import ast as A
from fractions import Fraction
from typing import Any, List, Tuple, Dict

from .lreader import Syn, Symbol, Keyword, SrcSpan
from .builtins import _pvec, _pmap, _pset, _plist, _ratio, _kw, _sym, mangle_symbol, loop, tail_recursive
from .macros import expand_macros, parse_syntax_rules, MacroError
from .python_imports import import_python_module, import_python_from, export_to_python, create_python_module

__all__ = [
    "Compiler", "compile_module"
]

class CompileError(Exception):
    pass

class Compiler:
    def __init__(self):
        self.expr_counter = 0
        self.module_body: List[A.stmt] = []

    def compile_toplevel(self, syn: Syn) -> str:
        node = self.compile_expr(syn)
        name = f"__expr{self.expr_counter}"
        self.expr_counter += 1
        assign = A.Assign(targets=[A.Name(id=name, ctx=A.Store())], value=node)
        self._set_span(assign, syn.span)
        self.module_body.append(assign)
        return name

    def compile_expr(self, syn: Syn) -> A.expr:
        # Expand macros first
        expanded_syn = expand_macros(syn)
        
        t, v = expanded_syn.tag, expanded_syn.val
        if t == "number":
            if isinstance(v, Fraction):
                call = A.Call(A.Name("_ratio", A.Load()), [A.Constant(v.numerator), A.Constant(v.denominator)], [])
                return self._with_span(call, syn.span)
            return self._with_span(A.Constant(v), syn.span)
        if t == "string": return self._with_span(A.Constant(v), syn.span)
        if t == "boolean": return self._with_span(A.Constant(v), syn.span)
        if t == "nil": return self._with_span(A.Constant(None), syn.span)
        if t == "keyword":
            call = A.Call(A.Name("_kw", A.Load()), [A.Constant(v.qual)], [])
            return self._with_span(call, syn.span)
        if t == "symbol":
            ident = mangle_symbol(v)
            return self._with_span(A.Name(ident, A.Load()), syn.span)
        if t == "quote":
            return self.compile_quoted(expanded_syn)
        if t == "list":
            return self.compile_list(expanded_syn)
        if t == "vector":
            elts = [self.compile_expr(e) for e in v]
            call = A.Call(A.Name("_pvec", A.Load()), [A.List(elts=elts, ctx=A.Load())], [])
            return self._with_span(call, syn.span)
        if t == "map":
            tup_elts = [A.Tuple(elts=[self.compile_expr(k), self.compile_expr(val)], ctx=A.Load()) for (k, val) in v]
            call = A.Call(A.Name("_pmap", A.Load()), [A.List(elts=tup_elts, ctx=A.Load())], [])
            return self._with_span(call, syn.span)
        if t == "set":
            elts = [self.compile_expr(e) for e in v]
            call = A.Call(A.Name("_pset", A.Load()), [A.List(elts=elts, ctx=A.Load())], [])
            return self._with_span(call, syn.span)
        if t == "tuple":
            elts = [self.compile_expr(e) for e in v]
            return self._with_span(A.Tuple(elts=elts, ctx=A.Load()), syn.span)
        if t == "import-module":
            return self._compile_import_module(v, syn.span)
        if t == "import-from":
            return self._compile_import_from(v, syn.span)
        if t == "export":
            return self._compile_export(v, syn.span)
        if t == "dot":
            return self._compile_dot(v, syn.span)
        raise CompileError(f"cannot compile tag: {t}")

    def compile_quoted(self, syn: Syn) -> A.expr:
        def q(node: Syn) -> A.expr:
            t, v = node.tag, node.val
            if t == "number":
                if isinstance(v, Fraction):
                    return A.Call(A.Name("_ratio", A.Load()), [A.Constant(v.numerator), A.Constant(v.denominator)], [])
                return A.Constant(v)
            if t == "string": return A.Constant(v)
            if t == "boolean": return A.Constant(v)
            if t == "nil": return A.Constant(None)
            if t == "keyword": return A.Call(A.Name("_kw", A.Load()), [A.Constant(v.qual)], [])
            if t == "symbol": return A.Call(A.Name("_sym", A.Load()), [A.Constant(v.qual)], [])
            if t == "list": return A.Call(A.Name("_plist", A.Load()), [A.List(elts=[q(e) for e in v])], [])
            if t == "vector": return A.Call(A.Name("_pvec", A.Load()), [A.List(elts=[q(e) for e in v])], [])
            if t == "map":
                pairs = [A.Tuple(elts=[q(k), q(val)], ctx=A.Load()) for (k, val) in v]
                return A.Call(A.Name("_pmap", A.Load()), [A.List(elts=pairs, ctx=A.Load())], [])
            if t == "set": return A.Call(A.Name("_pset", A.Load()), [A.List(elts=[q(e) for e in v])], [])
            if t == "tuple": return A.Tuple(elts=[q(e) for e in v], ctx=A.Load())
            raise CompileError(f"quote: cannot construct for {t}")
        return self._with_span(q(syn.val), syn.span)

    def compile_list(self, syn: Syn) -> A.expr:
        items: List[Syn] = syn.val
        if not items: return self._with_span(A.Tuple(elts=[], ctx=A.Load()), syn.span)
        head = items[0]
        # If the list is a single dot form like '(. obj m1 m2)', compile the dot directly without application
        if len(items) == 1 and head.tag == "dot":
            return self.compile_expr(head)
        if head.tag == "symbol":
            name = head.val.qual
            # Treat (. obj m1 m2 ...) specially even if it comes in as a list
            if name == ".":
                if len(items) < 3:
                    raise CompileError("(. object member ...) requires at least one member")
                obj_syn = items[1]
                members: List[str] = []
                for m in items[2:]:
                    if m.tag == "symbol":
                        members.append(m.val.qual)
                    elif m.tag == "string":
                        members.append(m.val)
                    else:
                        raise CompileError("dot members must be symbols or strings")
                return self._compile_dot({"object": obj_syn, "members": members}, syn.span)
            if name == "if": return self._compile_if(items, syn.span)
            if name == "begin": return self._compile_begin(items, syn.span)
            if name == "define": return self._compile_define(items, syn.span)
            if name == "define-syntax": return self._compile_define_syntax(items, syn.span)
            if name == "lambda": return self._compile_lambda(items, syn.span)
            if name == "let": return self._compile_let_desugared(items, syn.span)
            if name == "set!": return self._compile_setbang(items, syn.span)
            if name == "loop": return self._compile_loop(items, syn.span)
        fn_ast = self.compile_expr(items[0])
        # Support keyword arguments: (f a b :kw1 v1 :kw2 v2)
        args_ast: List[A.expr] = []
        keywords_ast: List[A.keyword] = []
        i = 1
        while i < len(items):
            arg_syn = items[i]
            if arg_syn.tag == "keyword":
                if i + 1 >= len(items):
                    raise CompileError("keyword argument missing value")
                kw_name = arg_syn.val.qual
                kw_value = self.compile_expr(items[i + 1])
                keywords_ast.append(A.keyword(arg=kw_name, value=kw_value))
                i += 2
            else:
                args_ast.append(self.compile_expr(arg_syn))
                i += 1
        call = A.Call(func=fn_ast, args=args_ast, keywords=keywords_ast)
        return self._with_span(call, syn.span)

    def _compile_if(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) not in (3, 4): raise CompileError("(if test then [else])")
        test = self.compile_expr(items[1])
        conseq = self.compile_expr(items[2])
        alt = self.compile_expr(items[3]) if len(items) == 4 else A.Constant(None)
        node = A.IfExp(test=test, body=conseq, orelse=alt)
        return self._with_span(node, span)

    def _compile_begin(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) == 1: return self._with_span(A.Constant(None), span)
        lam = A.Lambda(
            args=A.arguments(posonlyargs=[], args=[], kwonlyargs=[], kw_defaults=[], defaults=[], vararg=None, kwarg=None),
            body=self.compile_expr(items[-1])
        )
        return self._with_span(A.Call(func=lam, args=[], keywords=[]), span)

    def _compile_define(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) < 3: raise CompileError("(define name expr) or (define (f args) body...) ")
        target = items[1]
        if target.tag == "list":
            lst = target.val
            if not lst or lst[0].tag != "symbol": raise CompileError("function name must be a symbol")
            fname = lst[0].val
            params = lst[1:]
            
            # Check if the function body contains recur calls
            body_forms = items[2:]
            needs_tail_recursion = any(contains_recur(form) for form in body_forms)
            
            # Also check if any lambda body contains recur calls
            for form in body_forms:
                if form.tag == "list" and form.val and form.val[0].tag == "symbol" and form.val[0].val.qual == "lambda":
                    lambda_body = form.val[2:]
                    if any(contains_recur(body_form) for body_form in lambda_body):
                        needs_tail_recursion = True
                        break
            

            
            lam = self._lambda_from_parts(params, body_forms)
            
            # If the function contains recur calls, wrap it with tail_recursive
            if needs_tail_recursion:
                lam = A.Call(
                    func=A.Name("tail_recursive", A.Load()),
                    args=[lam],
                    keywords=[]
                )
            
            assign = A.Assign(targets=[A.Name(mangle_symbol(fname), A.Store())], value=lam)
            self._set_span(assign, span)
            self.module_body.append(assign)
            return self._with_span(A.Name(mangle_symbol(fname), A.Load()), span)
        elif target.tag == "symbol":
            expr = self.compile_expr(items[2])
            assign = A.Assign(targets=[A.Name(mangle_symbol(target.val), A.Store())], value=expr)
            self._set_span(assign, span)
            self.module_body.append(assign)
            return self._with_span(expr, span)
        else:
            raise CompileError("bad define target")

    def _compile_define_syntax(self, items: List[Syn], span: SrcSpan) -> A.expr:
        """Compile define-syntax form"""
        if len(items) != 3:
            raise CompileError("(define-syntax name syntax-rules)")
        
        name_expr = items[1]
        syntax_rules_expr = items[2]
        
        if name_expr.tag != "symbol":
            raise CompileError("define-syntax name must be a symbol")
        
        if (syntax_rules_expr.tag != "list" or 
            not syntax_rules_expr.val or 
            syntax_rules_expr.val[0].tag != "symbol" or
            syntax_rules_expr.val[0].val.qual != "syntax-rules"):
            raise CompileError("define-syntax body must be syntax-rules")
        
        try:
            # Parse the syntax-rules
            literals, rules = parse_syntax_rules(syntax_rules_expr)
            
            # Define the macro
            from .macros import define_syntax_rules
            define_syntax_rules(name_expr.val.qual, literals, rules)
            
            # Return a no-op expression (macro definition is handled at compile time)
            return self._with_span(A.Constant(None), span)
            
        except MacroError as e:
            raise CompileError(f"macro definition error: {e}")

    def _compile_dot(self, dot_data: Dict[str, Any], span: SrcSpan) -> A.expr:
        """Compile (. object m1 m2 ...) form to dot_access(object, m1, m2, ...)"""
        obj = self.compile_expr(dot_data["object"])
        members = dot_data["members"]
        
        # Create a function call to dot_access with the object and all member names
        args = [obj] + [A.Constant(member) for member in members]
        call = A.Call(
            func=A.Name("dot_access", A.Load()),
            args=args,
            keywords=[]
        )
        
        return self._with_span(call, span)

    def _compile_import_module(self, import_data: Dict[str, Any], span: SrcSpan) -> A.expr:
        """Compile #import(module [as alias]) form"""
        module_name = import_data["module"]
        alias = import_data.get("alias")
        import_all = import_data.get("all", False)
        
        # Call import_python_module function
        args = [A.Constant(module_name)]
        if alias:
            args.append(A.Constant(alias))
        
        call = A.Call(
            func=A.Name("import_python_module", A.Load()),
            args=args,
            keywords=[A.keyword(arg="import_all", value=A.Constant(True))] if import_all else []
        )
        
        return self._with_span(call, span)

    def _compile_import_from(self, import_data: Dict[str, Any], span: SrcSpan) -> A.expr:
        """Compile (import module (entity1 entity2 ...) [:as alias]) form"""
        module_name = import_data["module"]
        entities = import_data["entities"]
        entity_aliases = import_data.get("entity_aliases", {})
        module_alias = import_data.get("module_alias")
        
        # Build entity names and aliases
        entity_names = []
        alias_names = []
        
        for entity_name in entities:
            entity_names.append(entity_name)
            if entity_name in entity_aliases:
                alias_names.append(entity_aliases[entity_name])
            else:
                alias_names.append(entity_name)
        
        # Call import_python_from function
        args = [
            A.Constant(module_name),
            A.List(elts=[A.Constant(name) for name in entity_names], ctx=A.Load()),
            A.List(elts=[A.Constant(name) for name in alias_names], ctx=A.Load())
        ]
        
        call = A.Call(
            func=A.Name("import_python_from", A.Load()),
            args=args,
            keywords=[]
        )
        
        return self._with_span(call, span)

    def _compile_export(self, export_data: Dict[str, Any], span: SrcSpan) -> A.expr:
        """Compile #export(name [as python_name] [to module_name]) form"""
        name = export_data["name"]
        python_name = export_data.get("python_name")
        module_name = export_data.get("module_name", "lisp")
        
        # Call export_to_python function
        args = [
            A.Constant(name),
            A.Name(mangle_symbol(Symbol(name)), A.Load()),  # The actual entity to export
            A.Constant(module_name)
        ]
        
        if python_name:
            args[0] = A.Constant(python_name)  # Use the Python name instead
        
        call = A.Call(
            func=A.Name("export_to_python", A.Load()),
            args=args,
            keywords=[]
        )
        
        return self._with_span(call, span)

    def _compile_lambda(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) < 3: raise CompileError("(lambda (args) body...) ")
        params = items[1].val if items[1].tag == "list" else []
        body_forms = items[2:]
        
        # Check if the lambda body contains recur calls
        needs_tail_recursion = any(contains_recur(form) for form in body_forms)
        
        lam = self._lambda_from_parts(params, body_forms)
        
        # If the lambda contains recur calls, wrap it with tail_recursive
        if needs_tail_recursion:
            lam = A.Call(
                func=A.Name("tail_recursive", A.Load()),
                args=[lam],
                keywords=[]
            )
        
        return self._with_span(lam, span)

    def _lambda_from_parts(self, params: List[Syn], body_forms: List[Syn]):
        args: List[A.arg] = []
        for p in params:
            if p.tag != "symbol": raise CompileError("lambda parameters must be symbols")
            name = mangle_symbol(p.val)
            args.append(A.arg(arg=name))
        if not body_forms:
            body_ast = A.Constant(None)
        elif len(body_forms) == 1:
            body_ast = self.compile_expr(body_forms[0])
        else:
            body_ast = self.compile_expr(Syn("begin", body_forms, body_forms[0].span))
        return A.Lambda(args=A.arguments(posonlyargs=[], args=args, kwonlyargs=[], kw_defaults=[], defaults=[], vararg=None, kwarg=None), body=body_ast)

    def _compile_setbang(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) != 3 or items[1].tag != "symbol": raise CompileError("(set! name expr)")
        target = A.Name(mangle_symbol(items[1].val), A.Store())
        value = self.compile_expr(items[2])
        assign = A.Assign(targets=[target], value=value)
        self._set_span(assign, span)
        self.module_body.append(assign)
        return self._with_span(A.Name(mangle_symbol(items[1].val), A.Load()), span)

    def _compile_let_desugared(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) < 3 or items[1].tag != "list": raise CompileError("(let ((x e) ...) body...) ")
        binds = items[1].val
        params: List[Syn] = []; args: List[Syn] = []
        for b in binds:
            if b.tag != "list" or len(b.val) != 2 or b.val[0].tag != "symbol": raise CompileError("let binding must be (name expr)")
            params.append(b.val[0]); args.append(b.val[1])
        lam = Syn("list", [Syn("symbol", _sym("lambda"), span), Syn("list", params, span)] + items[2:], span)
        call = Syn("list", [lam] + args, span)
        return self.compile_expr(call)

    def _compile_loop(self, items: List[Syn], span: SrcSpan) -> A.expr:
        if len(items) < 3: raise CompileError("(loop [bindings...] body...)")
        
        # Parse bindings
        bindings_expr = items[1]
        if bindings_expr.tag != "list":
            raise CompileError("loop bindings must be a list")
        
        bindings = []
        for binding in bindings_expr.val:
            if binding.tag != "list" or len(binding.val) != 2:
                raise CompileError("loop binding must be (name value)")
            name_syn, value_syn = binding.val[0], binding.val[1]
            if name_syn.tag != "symbol":
                raise CompileError("loop binding name must be a symbol")
            bindings.append((name_syn.val.qual, self.compile_expr(value_syn)))
        
        # Compile body
        if len(items) == 2:
            body_ast = A.Constant(None)
        elif len(items) == 3:
            body_ast = self.compile_expr(items[2])
        else:
            body_ast = self.compile_expr(Syn("begin", items[2:], items[2].span))
        
        # Create a lambda that takes the bindings as arguments
        binding_names = [name for name, _ in bindings]
        binding_values = [value for _, value in bindings]
        
        # Create the loop function call
        # We need to pass a list of (name, value) tuples to the loop function
        binding_tuples = []
        for name, value in bindings:
            binding_tuples.append(A.Tuple(
                elts=[A.Constant(value=name), value],
                ctx=A.Load()
            ))
        
        # Create a wrapper that provides the variables in the environment
        # and compiles the body with proper variable access
        def compile_with_env(syn, env_name="env"):
            """Compile a syntax node with environment variable access"""
            if syn.tag == "symbol":
                # Check if this is a loop variable
                if syn.val.qual in binding_names:
                    # Access variable from environment
                    return A.Subscript(
                        value=A.Name(env_name, A.Load()),
                        slice=A.Constant(value=syn.val.qual),
                        ctx=A.Load()
                    )
                else:
                    # Use normal compilation for non-loop variables
                    return self.compile_expr(syn)
            elif syn.tag == "list":
                # Handle special forms and function calls
                items = syn.val
                if not items:
                    return A.List(elts=[], ctx=A.Load())
                
                head = items[0]
                if head.tag == "symbol":
                    name = head.val.qual
                    # Handle special forms
                    if name == "if":
                        if len(items) not in (3, 4): 
                            raise CompileError("(if test then [else])")
                        test = compile_with_env(items[1], env_name)
                        conseq = compile_with_env(items[2], env_name)
                        alt = compile_with_env(items[3], env_name) if len(items) == 4 else A.Constant(None)
                        return A.IfExp(test=test, body=conseq, orelse=alt)
                    elif name == "recur":
                        # Handle recur specially - it should call the recur function in the environment
                        args = [compile_with_env(arg, env_name) for arg in items[1:]]
                        return A.Call(
                            func=A.Subscript(
                                value=A.Name(env_name, A.Load()),
                                slice=A.Constant(value="recur"),
                                ctx=A.Load()
                            ),
                            args=args,
                            keywords=[]
                        )
                    else:
                        # Regular function call
                        fn_ast = compile_with_env(items[0], env_name)
                        args_ast = [compile_with_env(a, env_name) for a in items[1:]]
                        return A.Call(func=fn_ast, args=args_ast, keywords=[])
                else:
                    # Head is not a symbol, compile normally
                    fn_ast = compile_with_env(items[0], env_name)
                    args_ast = [compile_with_env(a, env_name) for a in items[1:]]
                    return A.Call(func=fn_ast, args=args_ast, keywords=[])
            else:
                # For other types, use normal compilation
                return self.compile_expr(syn)
        
        # Compile the body with environment access
        if len(items) == 2:
            body_ast = A.Constant(None)
        elif len(items) == 3:
            body_ast = compile_with_env(items[2])
        else:
            # For multiple body expressions, compile them as a begin
            body_exprs = [compile_with_env(item) for item in items[2:]]
            if len(body_exprs) == 1:
                body_ast = body_exprs[0]
            else:
                # Create a begin-like structure
                body_ast = A.Call(
                    func=A.Lambda(
                        args=A.arguments(posonlyargs=[], args=[], kwonlyargs=[], kw_defaults=[], defaults=[], vararg=None, kwarg=None),
                        body=body_exprs[-1]
                    ),
                    args=[],
                    keywords=[]
                )
        
        # Create the loop function call
        loop_call = A.Call(
            func=A.Name("loop", A.Load()),
            args=[
                A.List(elts=binding_tuples, ctx=A.Load()),
                A.Lambda(
                    args=A.arguments(
                        posonlyargs=[],
                        args=[A.arg(arg="env")],
                        kwonlyargs=[],
                        kw_defaults=[],
                        defaults=[],
                        vararg=None,
                        kwarg=None
                    ),
                    body=body_ast
                )
            ],
            keywords=[]
        )
        
        return self._with_span(loop_call, span)

    def _with_span(self, node: A.AST, span: SrcSpan) -> A.AST:
        self._set_span(node, span); return node

    def _set_span(self, node: A.AST, span: SrcSpan):
        for n in A.walk(node):
            if not hasattr(n, 'lineno') or n.lineno is None:
                n.lineno = span.start_line; n.col_offset = span.start_col
            if not hasattr(n, 'end_lineno') or n.end_lineno is None:
                n.end_lineno = span.end_line; n.end_col_offset = span.end_col


def contains_recur(syn: Syn) -> bool:
    """Check if a syntax tree contains any recur calls"""
    if syn.tag == "list":
        items = syn.val
        if items and items[0].tag == "symbol" and items[0].val.qual == "recur":
            return True
        return any(contains_recur(item) for item in items)
    return False


def compile_module(forms: List[Syn]) -> A.Module:
    c = Compiler()
    for f in forms:
        c.compile_toplevel(f)
    return A.Module(body=c.module_body, type_ignores=[])