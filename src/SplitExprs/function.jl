"
A data representation of a function definition, or lambda,
    comparable to `MacroTools.splitdef()` but far more thorough.

You can also split/combine a function call, represented by a `nothing` body,
    but must turn off strict mode (pass `false` in the constructor)
    to successfully parse literal-valued parameters.

A lambda is represented by a `nothing` name.
Setting both the body and name to `nothing` is not allowed.
"
mutable struct SplitFunction <: AbstractSplitExpr
    name::Optional # `nothing` if this is a lambda
    args::Vector{SplitArg}
    kw_args::Vector{SplitArg}
    body::Optional # `nothing` if this is a function call
    return_type::Optional # `nothing` if not given
    where_params::Vector{SplitType}
    doc_string::Optional{AbstractString}
    inline::Bool
    generated::Bool
    is_escaped::Bool
end

function SplitFunction(expr, strict_mode::Bool = false)::Optional{SplitFunction}
    is_escaped::Bool = isexpr(expr, :escape)
    if is_escaped
        expr = expr.args[1]
    end

    metadata = SplitMeta(expr)
    if isnothing(metadata)
        return nothing
    end
    expr = metadata.core_expr

    # If it's just a function call, give it a Nothing body.
    if !isexpr(expr, :->) && !MacroTools.isshortdef(MacroTools.shortdef(expr))
        expr = :( $expr = $nothing )
    end

    # Check that the expr under the metadata is really a function definition.
    #NOTE: this snippet comes from MacroTools source;
    #    unfortunately it isn't available as a helper function
    if !@capture(longdef(expr), function (fcall_ | fcall_) body_ end)
        return nothing
    end

    dict = MacroTools.splitdef(expr)

    # Check for invalid data.
    args = SplitArg.(dict[:args], Ref(strict_mode))
    kw_args = SplitArg.(dict[:kwargs], Ref(strict_mode))
    type_args = SplitType.(dict[:whereparams], Ref(strict_mode))
    if any(isnothing, args) || any(isnothing, kw_args) || any(isnothing, type_args)
        return nothing
    end

    data = SplitFunction(
        get(dict, :name, nothing),
        collect(args), collect(kw_args),
        dict[:body],
        get(dict, :rtype, nothing),
        collect(type_args),
        metadata.doc_string, metadata.inline, metadata.generated,
        is_escaped
    )
    return data
end

SplitFunction(s::SplitFunction, copy_body::Bool = true) = SplitFunction(
    expr_deepcopy(s.name),
    SplitArg.(s.args),
    SplitArg.(s.kw_args),
    copy_body ? expr_deepcopy(s.body) : s.body,
    expr_deepcopy(s.return_type),
    SplitType.(s.where_params),
    expr_deepcopy(s.doc_string),
    s.inline, s.generated, s.is_escaped
)

function combine_expr(sf::SplitFunction)
    if isnothing(sf.body) # A function call?
        if isnothing(sf.name)
            error("A function call/signature must have a name")
        end

        # Ordered and named parameters look identical in the AST.
        # For ordered parameters, we want to replace `a::T = v` with `a`.
        # For named parameters, we want to replace `a::T = v` with `a=a`.
        args = map(sf.args) do arg
            return combine_expr(
                SplitArg(arg.name, nothing, arg.is_splat, nothing, arg.is_escaped)
            )
        end
        kw_args = map(sf.kw_args) do arg
            return combine_expr(if arg.is_splat
                SplitArg(arg.name, nothing, true, nothing, arg.is_escaped)
            else
                SplitArg(arg.name, nothing, false, arg.name, arg.is_escaped)
            end)
        end

        raw_expr = Expr(:call,
            sf.name,
            (isempty(kw_args) ? tuple() : tuple(Expr(:parameters, kw_args...))),
            args...
        )
        return sf.is_escaped ? esc(raw_expr) : raw_expr
    else # A function definition?
        dict = Dict(
            :args => combine_expr.(sf.args),
            :kwargs => combine_expr.(sf.kw_args),
            :body => sf.body,
            :whereparams => combine_expr.(sf.where_params),
        )
        exists(sf.name)        &&    (dict[:name] = sf.name)
        exists(sf.return_type) &&    (dict[:rtype] = sf.return_type)

        definition = combinedef(dict)
        return combine_expr(SplitMeta(
            sf.doc_string,
            sf.inline,
            sf.generated,
            definition,
            sf.is_escaped
        ))
    end
end