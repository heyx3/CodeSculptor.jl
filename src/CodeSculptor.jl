module CodeSculptor

using Setfield, MacroTools

const Optional{T} = Union{Nothing, T}
@inline exists(t) = !isnothing(t)


include("utility_functions.jl")
include("operators.jl")


"
Some data representation of a particular kind of expression.

* Turn it back into the original expression with `combine_expr(e)`.
* Check if the entire split expression is escaped with `is_escaped(e)` (does not check for inner things being escaped).
* Make a smart deep-copy (using `expr_deepcopy()`) by constructing another one and passing the original.

When applicable, constructors return `nothing` if the expression cannot be split,
    but less strict interpretation can be requested by passing an extra parameter `false`.
"
abstract type AbstractSplitExpr end
combine_expr(a::AbstractSplitExpr) = error("combine_expr(::", typeof(a), ") not implemented")
is_escaped(a::AbstractSplitExpr)::Bool = a.is_escaped


include("SplitExprs/meta.jl")
include("SplitExprs/arg.jl")
include("SplitExprs/type.jl")
include("SplitExprs/function.jl")
include("SplitExprs/macro.jl")


export function_wrapping_is_valid,
       is_scopable_name, is_function_call,
       is_short_function_decl, is_function_decl,
       is_macro_invocation,
       visit_exprs, expr_deepcopy,
       unescape, unescape_deep,
       ASSIGNMENT_INNER_OP, ASSIGNMENT_WITH_OP,
       compute_op,
       AbstractSplitExpr, combine_expr, is_escaped,
       SplitMeta, SplitArg, SplitType, SplitFunction, SplitMacro

end # module MacroCraft
