"
Information that sits outside of a definition (especially a function definition).
For example `@inline`, `@generated`, and doc-strings.
"
mutable struct SplitMeta <: AbstractSplitExpr
    doc_string::Optional{AbstractString}
    inline::Bool
    generated::Bool
    core_expr # The actual function definition with everything stripped out
    is_escaped::Bool # Considered to be around the core_expr, even if it was originally outside
end

"Returns `nothing` if the metadata can't be extracted (i.e. it's wrapped by an unexpected macro)"
function SplitMeta(expr)::Optional{SplitMeta}
    # Strip the outer macro call.
    if @capture expr (@name_ args__)
        if name == GlobalRef(Core, Symbol("@doc"))
            inner = SplitMeta(args[2])
            inner.doc_string = args[1]
            return inner
        elseif name == Symbol("@inline")
            inner = SplitMeta(args[1])
            inner.inline = true
            return inner
        elseif name == Symbol("@generated")
            inner = SplitMeta(args[1])
            inner.generated = true
            return inner
        else
            return nothing
        end
    # Strip the esc() wrapper.
    elseif MacroTools.isexpr(expr, :escape)
        inner = SplitMeta(expr.args[1])
        inner.is_escaped = true
        return inner
    # Return a basic metadata object.
    else
        return SplitMeta(nothing, false, false, expr, false)
    end
end
function SplitMeta(m::SplitMeta, copy_core_expr::Bool = true)
    return SplitMeta(
        copy(m.doc_string),
        m.inline, m.generated,
        copy_core_expr ? expr_deepcopy(m.core_expr) : m.core_expr,
        m.is_escaped
    )
end

function combine_expr(m::SplitMeta)
    output = m.core_expr
    is_escaped(m)        &&   (output = esc(output))
    m.generated          &&   (output = :( @generated $output ))
    m.inline             &&   (output = :( @inline $output ))
    exists(m.doc_string) &&   (output = :( Core.@doc($(m.doc_string), $output) ))
    return output
end