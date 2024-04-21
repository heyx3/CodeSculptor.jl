"A data representation of a type declaration, such as `C{R, T<:Integer} <: B`"
mutable struct SplitType <: AbstractSplitExpr
    name # Must be a Symbol in 'strict' mode
    type_params::Vector # Elements must be Symbol or '[Symbol]<:expr' in strict mode.
                        # Usually the expr will be a scoped name, but technically could be any expression.
    parent::Optional # Usually a scoped name (e.x. 'A' or 'M1.M2.A'),
                     #    but technically could be any expression.
    is_escaped::Bool
end

function SplitType(expr, strict_mode::Bool = true)::Optional{SplitType}
    is_escaped = isexpr(expr, :escape)
    if is_escaped
        expr = expr.args[1]
    end

    local output::SplitType
    if @capture(expr, n_{t__} <: b_)
        output = SplitType(n, t, b, is_escaped)
    elseif @capture(expr, n_{t__})
        output = SplitType(n, t, b, is_escaped)
    elseif @capture(expr, n_<:b_)
        output = SplitType(n, [], b, is_escaped)
    else
        output = SplitType(expr, [], nothing, is_escaped)
    end

    if strict_mode
        if !isa(output.name, Symbol)
            return nothing
        end
        for tt in output.type_params
            if !(tt isa Symbol) &&
               (!isexpr(tt, :<:) || (length(tt.args) != 2) || !(tt.args[1] isa Symbol))
            #begin
                return nothing
            end
        end
        if !(output.parent isa Union{Nothing, Symbol, Expr})
            return nothing
        end
    end

    return output
end
SplitType(src::SplitType) = SplitType(
    expr_deepcopy(src.name),
    expr_deepcopy.(src.type_params),
    expr_deepcopy(src.parent),
    src.is_escaped
)

function combine_expr(st::SplitType)
    raw_expr =
        if isempty(st.type_params)
            if isnothing(st.parent)
                st.name
            else
                :( $(st.name) <: $(st.parent) )
            end
        else
            if isnothing(st.parent)
                :( $(st.name){$(st.type_params...)} )
            else
                :( $(st.name){$(st.type_params...)} <: $(st.parent) )
            end
        end
    return st.is_escaped ? esc(raw_expr) : raw_expr
end