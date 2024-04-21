"
A data represenation of an argument declaration, comparable to the output of `MacroTools.splitarg()`
    but handling extra stuff like being wrapped in an `esc()` call.
"
mutable struct SplitArg <: AbstractSplitExpr
    name # Almost always a Symbol, but technically could be other syntax structures (like 'esc()').
    type::Optional # Usually a Symbol or scoped name like `Random.AbstractRNG`.
                   # If no type was given, or the type was ':Any', this will be set to 'nothing'.
    is_splat::Bool # Does it end in a '...'?
    default_value::Optional # `nothing` if not given.
    is_escaped::Bool # If true, the whole thing was wrapped in an 'esc()'.
end

function SplitArg(expr, strict_mode::Bool = true)::Optional{SplitArg}
    val = if isexpr(expr, :escape)
        SplitArg(splitarg(expr.args[1])..., true)
    else
        SplitArg(splitarg(expr)..., false)
    end
    # Sanitize the return type.
    if (val.type == :Any)
        val.type = nothing
    end
    # Check for invalid format.
    #TODO: Unit-test for this case
    if strict_mode && !isa(unescape(val.name), Symbol)
        return nothing
    end
    return val
end
SplitArg(src::SplitArg) = SplitArg(
    expr_deepcopy(src.name), expr_deepcopy(src.type),
    src.is_splat, expr_deepcopy(src.default_value),
    src.is_escaped
)

function combine_expr(a::SplitArg)
    # MacroTools treats '::Any' and no typing as the same,
    #    so I'm ignoring their implementation.
    raw_expr =
        if exists(a.name)
            if exists(a.type)
                if exists(a.default_value)
                    if a.is_splat
                        Expr(:kw, :( $(a.name)::$(a.type)... ), a.default_value)
                    else
                        Expr(:kw, :( $(a.name)::$(a.type) ), a.default_value)
                    end
                else
                    if a.is_splat
                        :( $(a.name)::$(a.type)... )
                    else
                        :( $(a.name)::$(a.type) )
                    end
                end
            else
                if exists(a.default_value)
                    if a.is_splat
                        Expr(:kw, :( $(a.name)... ), a.default_value)
                    else
                        Expr(:kw, a.name, a.default_value)
                    end
                else
                    if a.is_splat
                        :( $(a.name)... )
                    else
                        a.name
                    end
                end
            end
        else
            if exists(a.type)
                if exists(a.default_value)
                    if a.is_splat
                        Expr(:kw, :( ::$(a.type)... ), a.default_value)
                    else
                        Expr(:kw, :( ::$(a.type) ), a.default_value)
                    end
                else
                    if a.is_splat
                        :( ::$(a.type)... )
                    else
                        :( ::$(a.type) )
                    end
                end
            else
                if exists(a.default_value)
                    if a.is_splat
                        Expr(:kw, :( _... ), a.default_value)
                    else
                        Expr(:kw, :_, a.default_value)
                    end
                else
                    if a.is_splat
                        :( _... )
                    else
                        :_
                    end
                end
            end
        end
    return if a.is_escaped
        esc(raw_expr)
    else
        raw_expr
    end
end