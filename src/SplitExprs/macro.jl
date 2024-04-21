"
A data representation of a macro invocation, such as `@assert x == 5`.
"
mutable struct SplitMacro <: AbstractSplitExpr
    name # A Symbol or scoped name (e.x. `Base.@assert`).
    source::LineNumberNode
    args::Vector
end

function SplitMacro(expr)::Optional{SplitMacro}
    if isexpr(expr, :macrocall)
        return SplitMacro(
            expr.args[1],
            expr.args[2],
            expr.args[3:end]
        )
    else
        return nothing
    end
end
SplitMacro(src::SplitMacro, deepcopy_args::Bool = true) = SplitMacro(
    expr_deepcopy(src.name),
    expr_deepcopy(src.source),
    deepcopy_args ? map(expr_deepcopy, src.args) : map(identity, src.args)
)

"Turns the data representaton of a macro call into an AST"
function combine_expr(m::SplitMacro)
    return Expr(:macrocall,
        m.name,
        m.source,
        m.args...
    )
end