# CodeSculptor

Split many kinds of expressions into detailed parts, play with them, and put them back together again.
Think of it as a really souped-up version of `splitdef()` and `splitarg()` from **MacroTools.jl**:

````
using CodeSculptor

expr = :( f(i::Int) = i )
def = SplitFunction(expr)

# Make the function generic
def.args[1].type = :I
push!(def.where_params, SplitType(:( I<:Integer )))

# Put it back together and evaluate
eval(combine_expr(def))

# Generate a function call of f
def.body = nothing
def.args[1] = SplitArg(5, false)
@assert eval(combine_expr(def)) === 5

# Check syntax by looking for a null result
is_type_expr(e) = (SplitType(e) !== nothing)
@assert is_type_expr(C <: Integer)
@assert is_type_expr(D)
@assert !is_type_expr(3 + 8)
````

* `SplitFunction` for function calls and definitions
* `SplitMacro` for macro invocations
* `SplitMeta` for metadata around a definition, such as a docstring or `@inline`
* `SplitType` for type declarations (like `C{R, T<:Integer} <: B`)
* `SplitArg` for function arguments (like `a::Int...`)

Each field on the `SplitX` types is documented with comments.

Each of them can return `nothing` when constructed, if the expression fails to parse.
The constructors can also run in a less strict mode by passing `false` into the constructor,
    for example to stop `SplitFunction` from verifying that a function name has valid syntax.

Each of them can be deep-copied by constructing with a source instance to copy from.
Some of them offer extra flags to skip copying of particularly large parts of the AST.

## Helper Functions

There are also several accompanying functions to query and modify expressions:

* `is_scopable_name(expr)` checks that the expression is either a Symbol or a series of Symbols separated by a `.` expression. Optionally allows type parameters at the end, like `A.B{C}`.
* `expr_deepcopy(e)` works like deepcopy, but without copying special literals/interpolated references (like a `Module`).
* `visit_exprs(e)` is like `MacroTools.postwalk()`, but without modifying anything and with extra provided data that tells you exactly where you are in the original AST.
* `unescape(e)` and `unescape_deep(e)` help remove `esc()` from expressions.

## Operators

There are some tools to map between operators and assignments (e.x. `+` and `+=`).

* `ASSIGNMENT_INNER_OP` maps assignment to operator
* `ASSIGNMENT_WITH_OP` maps operator to assignment
* `compute_op(s::Union{Symbol, Val}, a, b)` computes an assignment as an operator
  * For example, `compute_op(:+=, 3, 4)` returns `compute_op(Val(:+=), 3, 4)` returns `7`.

# TODO

* `SplitField` for struct fields (optionally supporting assigned initial values)
* Break apart `SplitFunction` into `SplitCall`, `SplitSignature`, and `SplitFuncDef`.