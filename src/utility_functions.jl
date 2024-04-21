
"
Checks whether an expression contains valid function metadata
  (doc-string, `@inline`, etc., or several at once, or none of them)
"
function_wrapping_is_valid(expr)::Bool = exists(SplitMeta(expr))


"
Checks that an expression is a Symbol, possibly nested within `.` operators.
For example, `Base.empty`.

Optionally allows type parameters at the end of the expression, like `:( A.B{C} )`.
"
function is_scopable_name(expr, allow_type_params::Bool = false)::Bool
    if isexpr(expr, :curly)
        if allow_type_params
            return is_scopable_name(expr.args[1])
        else
            return false
        end
    elseif expr isa Symbol
        return true
    else
        return isexpr(expr, :.) && (expr.args[2] isa QuoteNode)
    end
end

"Gets whether an expression looks like a function call"
function is_function_call(expr)::Bool
    return @capture(expr, (f_(i__)) |
                          (f_(i__; j__)) |
                          (f_(i__)::R_) |
                          (f_(i__; j__)::R_) |
                          (f_(i__) where {T__}) |
                          (f_(i__; j__) where {T__}) |
                          (f_(i__)::R_ where {T__}) |
                          (f_(i__; j__)::R_ where {T__})) &&
           is_scopable_name(f)
end

"
Checks if an expression is a short-form function declaration (like `f() = 5`).
Preferred to MacroTools' version of this function,
    because that one accepts AST's that are not actually functions.
"
function is_short_function_decl(expr)::Bool
    # Peel off the metadata.
    metadata = SplitMeta(expr)
    if isnothing(metadata)
        return false
    end
    expr = metadata.core_expr

    # Check that the grammar is correct.
    #TODO: Support operator-style declarations, such as (a::T + b::T) = T(a.i + b.i)
    if !isexpr(expr, :(=)) || !is_function_call(expr.args[1])
        return false
    end

    return isexpr(expr, :(=)) && is_function_call(expr.args[1])
end

"
Checks if an expression is a valid function declaration.
Note that MacroTools' version of this function is overly permissive.
"
is_function_decl(expr)::Bool = is_short_function_decl(shortdef(expr))

is_macro_invocation(expr) = isexpr(expr, :macrocall)


"Deep-copies an expression AST, except for things that should not be copied like literal modules"
expr_deepcopy(ast) = deepcopy(ast)
expr_deepcopy(e::Union{Module, GlobalRef, String, UnionAll, Type}) = e
expr_deepcopy(e::Expr) = Expr(e.head, expr_deepcopy.(e.args)...)


"""
Walks through an expression depth-first,
    invoking your lambda on every sub-expression with the following arguments:

1 .A list of integers representing the path to this sub-expression (for each Expr along the way, the index of its argument)

2. A list of the parents to this sub-expression, from the root down to the current expr.

For example, you could pass a lambda of the form `(path, exprs) -> let e = exprs[end] ... end`.

Like `MacroTools.postwalk`, but without modifying the expression
    and with more context about where each expression sits in the AST.
"""
visit_exprs(to_do, tree) = visit_exprs(to_do, tree, Int[ ], Any[ tree ])
visit_exprs(to_do, root, current_path, current_exprs) = to_do(current_path, current_exprs)
visit_exprs(to_do, root::Expr, current_path, current_exprs) = begin
    to_do(current_path, current_exprs)
    for i in 1:length(root.args)
        child_path = push!(copy(current_path), i)
        child_exprs = push!(copy(current_exprs), root.args[i])
        visit_exprs(to_do, root.args[i], child_path, child_exprs)
    end
end

"Unwraps `esc()` from the outside of an expression, if it exists. Also see `unescape_deep()`"
unescape(e) = e
unescape(e::Expr) = (e.head == :escape) ? e.args[1] : e

"Removes `esc()` from an entire AST, returning the sanitized copy. Also see `unescape()`"
unescape_deep(e) = MacroTools.postwalk(unescape, e)