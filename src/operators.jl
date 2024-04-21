"Maps a modifying assignment operator (like `*=`) to its underlying operator (like `*`)"
const ASSIGNMENT_INNER_OP = Dict(
    :+= => :+,
    :-= => :-,
    :*= => :*,
    :/= => :/,
    :^= => :^,
    :÷= => :÷,
    :%= => :%,

    :|= => :|,
    :&= => :&,

    :⊻= => :⊻,
    :<<= => :<<,
    :>>= => :>>,
)
"Converts an operator (like `*`) to its assignment operation (like `*=`)"
const ASSIGNMENT_WITH_OP = Dict(v => k for (k,v) in ASSIGNMENT_INNER_OP)

"
Computes one of the modifying assignments (`*=`, `&=`, etc) given it and its inputs.
Also implements `=` for completeness.
"
@inline compute_op(s::Symbol, a, b) = compute_op(Val(s), a, b)
@inline compute_op(::Val{:(=)}, a, b) = b
for (name, op) in ASSIGNMENT_INNER_OP
    @eval @inline compute_op(::Val{Symbol($(string(name)))}, a, b) = $op(a, b)
end