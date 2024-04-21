# Activate the package project, and include these test dependencies on top.
using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
insert!(LOAD_PATH, 1, ".")

using Test
using MacroTools, CodeSculptor


function test_split_arg(a, name, type, splat, default, escaped)
    @test a.name == name
    @test a.type == type
    @test a.is_splat == splat
    @test a.default_value == default
    @test a.is_escaped == escaped
end
function test_split_type(t, name, type_params, parent)
    @test t.name == name
    @test length(t.type_params) == length(type_params)
    for (actual, expected) in zip(t.type_params, type_params)
        @test actual == expected
    end
    @test t.parent == parent
end
function test_split_macro(m, name, args)
    @test m.name == name
    @test m.args == args
end
function test_split_function(a, name, args, kw_args, body, return_type,
                             where_params, doc_string, inline, generated, is_escaped)
    @test a.name == name

    @test length(a.args) == length(args)
    for (actual, expected) in zip(a.args, args)
        test_split_arg(actual, expected...)
    end

    @test length(a.kw_args) == length(kw_args)
    for (actual, expected) in zip(a.kw_args, kw_args)
        test_split_arg(actual, expected...)
    end

    @test length(a.where_params) == length(where_params)
    for (actual, expected) in zip(a.where_params, where_params)
        test_split_type(actual, expected...)
    end

    # Strip out the block and LineNumberNode from the body.
    if MacroTools.isexpr(a.body, :block)
        @test a.body.args[findfirst(e -> !isa(e, LineNumberNode), a.body.args)] == body
    else
        @test a.body == body
    end

    @test a.return_type == return_type
    @test a.doc_string == doc_string
    @test a.inline == inline
    @test a.generated == generated
    @test a.is_escaped == is_escaped
end

@testset "CodeSculptor" begin
    @testset "SplitArg" begin
        @testset "Parsing" begin
            SUCCESS_TESTS = [
                # (input, strict_mode, (expected_output_fields...), expected_code_if_different)
                (:a, true, (
                    :a, nothing, false, nothing, false
                ), nothing),

                (:( i=7 ), true, (
                    :i, nothing, false, 7, false
                ), Expr(:kw, :i, 7)),

                (:( i::Int ), true, (
                    :i, :Int, false, nothing, false
                ), nothing),

                (:( $(esc(:i))::Int = 10 ), true, (
                    esc(:i), :Int, false, 10, false
                ), Expr(:kw, :( $(esc(:i))::Int ), 10)),

                (esc(:( i::Int... )), true, (
                    :i, :Int, true, nothing, true
                ), nothing),

                (:( (a, b)::Int = nothing ), false, (
                    :(a, b), :Int, false, :nothing, false
                ), Expr(:kw, :( (a, b)::Int ), :nothing))
            ]
            @testset for (input, strict_mode, expected_fields, expected_different_output) in SUCCESS_TESTS
                actual = SplitArg(input, strict_mode)
                @test actual !== nothing
                test_split_arg(actual, expected_fields...)

                # Test its code output through literal comparison.
                expected_output = if isnothing(expected_different_output)
                    input
                else
                    expected_different_output
                end
                @test combine_expr(actual) == expected_output
            end
        end
        @testset "Failing to parse" begin
            FAILURE_TESTS = [
                # (input, use_strict_mode)
                (:( (a+b)::Int = nothing ), true)
            ]
            @testset for (input, strict_mode::Bool) in FAILURE_TESTS
                actual = SplitArg(input, strict_mode)
                @test actual === nothing
            end
        end
    end
    @testset "SplitType" begin
        @testset "Parsing" begin
            SUCCESS_TESTS = [
                # (input, strict_mode, (expected_output_fields...))

                (:A, true, (
                    :A, [ ], nothing
                )),

                (:( A{B, C<:D} <: E() ), true, (
                    :A, [ :B, :(C<:D) ], :( E() )
                )),

                (:( A.B{23} <: D ), false, (
                    :(A.B), [ 23 ], :D
                ))
            ]
            @testset for (input, strict_mode::Bool, expected_output_fields) in SUCCESS_TESTS
                actual = SplitType(input, strict_mode)
                @test actual !== nothing

                test_split_type(actual, expected_output_fields...)

                @test combine_expr(actual) == input
            end
        end
        @testset "Failing to parse" begin
            FAILURE_TESTS = [
                # (input, strict_mode)
                (:( f() = 5 ), true), # Wrong syntax structure
                (:( A.B{C} <: D), true), # No qualified names for the type
                (:( A{<:B} <: C ), true) # Type param has invalid syntax
            ]
            @testset for (input, strict_mode::Bool) in FAILURE_TESTS
                t = SplitType(input, strict_mode)
                @test t === nothing
            end
        end
    end
    @testset "SplitMacro" begin
        @testset "Parsing" begin
            SUCCESS_TESTS = [
                # (input, (expected_data...), expected_code_string)
                (:( @a(b, c) ), (Symbol("@a"), [ :b, :c ]), "@a b c")
            ]
            for (input, expected_fields, expected_code_string) in SUCCESS_TESTS
                actual = SplitMacro(input)
                @test actual !== nothing

                test_split_macro(actual, expected_fields...)

                @test string(combine_expr(actual)) == "$(actual.source) $expected_code_string"
            end
        end
        @testset "Failing to parse" begin
            FAILURE_TESTS = [
                :( f() = 5 )
            ]
            @testset for input in FAILURE_TESTS
                actual = SplitMacro(input)
                @test actual === nothing
            end
        end
    end
    #TODO: Test SplitMeta
    @testset "SplitFunction calls" begin
        TESTS = [
            # (input, use_named_param_names_only, expected_output)
            (
                :( +(4, 5) ),
                false,
                4+5
            ),
            (
                :( Base.titlecase("aB cD eF"; strict=false) ),
                false,
                "AB CD EF"
            )
        ]
        @testset for (input, named_param_names_only::Bool, expected_output) in TESTS
            sf = SplitFunction(input)
            e = combine_expr(sf, named_param_names_only)
            v = eval(e)
            @test v == expected_output
        end
    end
    @testset "SplitFunction" begin
        @testset "Parsing" begin
            SUCCESS_TESTS = [
                # (input, strict_mode, (expected_output_fields...), eval_code, eval_expected_result)
                # If it is a lambda, eval_code should treat it with the name 'f'.
                # If it is a call, the last two test values should be 'nothing'.

                (:( f() = 5 ), true, (
                    :f, [ ], [ ], 5, nothing, [ ], nothing, false, false, false
                ), :( f() ), 5),

                (:( @inline (def, ghi::Int) -> def*ghi ), true, (
                    nothing,
                    [
                        (:def, nothing, false, nothing, false),
                        (:ghi, :Int, false, nothing, false)
                    ],
                    [ ],
                    :( def * ghi ), nothing,
                    [ ],
                    nothing, true, false, false
                ), :( f(3.4, 10) ), 3.4*10),

                (:( @generated function ghi(jkl::T; $(esc(:mnop)), qrs=5) where {T<:Integer}
                        return :( jkl + convert(T, mnop*qrs) )
                    end
                  ), true, (
                    :ghi,
                    [
                        (:jkl, :T, false, nothing, false)
                    ],
                    [
                        (:mnop, nothing, false, nothing, true),
                        (:qrs, nothing, false, 5, false)
                    ],
                    :( return :( jkl + convert(T, mnop*qrs) ) ),
                    nothing,
                    [
                        (:T, [ ], :Integer)
                    ],
                    nothing, false, true, false
                ), :( ghi(UInt8(2); mnop=10) ), UInt8(2) + convert(UInt8, 10*5)),

                ((quote
                    "ABCdef"
                    @inline function ghi(jkl::T; mnop, qrs=5) where {T}
                        return jkl
                    end
                  end
                 ).args[2], true, (
                     :ghi,
                     [
                        (:jkl, :T, false, nothing, false)
                     ],
                     [
                         (:mnop, nothing, false, nothing, false),
                         (:qrs, nothing, false, 5, false)
                     ],
                     :( return jkl ),
                     nothing,
                     [
                        (:T, [ ], nothing)
                     ],
                     "ABCdef", true, false, false
                 ), :( ghi("a", mnop=:hey_there) ), "a"),

                 (esc(:( abc(def, ghi::Int...)::Float32 )), true, (
                     :abc,
                     [
                        (:def, nothing, false, nothing, false),
                        (:ghi, :Int, true, nothing, false)
                     ],
                     [ ],
                     nothing, :Float32,
                     [ ], nothing, false, false,
                     true
                 ), nothing, nothing)
            ]
            @testset for (input, strict_mode::Bool, expected_fields, eval_code, expected_eval_result) in SUCCESS_TESTS
                f = SplitFunction(input)
                @test f !== nothing

                test_split_function(f, expected_fields...)

                # Test the generated code by eval-ing it.
                if !isnothing(f.body)
                    if isnothing(f.name)
                        f.name = :f
                    end
                    actual_eval_result = eval(quote
                        $(unescape_deep(combine_expr(f)))
                        $eval_code
                    end)
                    @test expected_eval_result == actual_eval_result
                end
            end
        end
        @testset "Failing to parse" begin
            FAILURE_TESTS = [
                # (input, strict_mode)
                (:( @someMacro ), false), # Fail to get metadata
                (:( 5 ), false), # Fail to be parsed as function
                (:( hey_there ), false), # Fail to be parsed as function

                (:( (34)(i::Int) = i ), true), # Name isn't scoped symbol

                (:( f(23) ), true),
                (:( f(; 25) ), true),
                (:( f() where {27} ), true),
            ]
            @testset for (input, strict_mode::Bool) in FAILURE_TESTS
                actual = SplitFunction(input, strict_mode)
                @test actual === nothing
            end
        end
    end
    @testset "Helper functions" begin
        @testset "is_function_decl()" begin
            function check_decl(expr, expected=true)
                @testset let info = "$((expected ? "Should" : "Should not")) be a function decl: $expr"
                    @test is_function_decl(expr) == expected
                end
            end
            function check_signature(signature, signature_is_valid=true)
                check_decl(signature, false)
                check_decl(:( $signature = nothing ), signature_is_valid)
            end
            function check_outer_signature(inner_signature, inner_is_valid=true)
                check_signature(inner_signature,                                   inner_is_valid)
                check_signature(:( $inner_signature::Int ),                        inner_is_valid)
                check_signature(:( $inner_signature::T where {T}),                 inner_is_valid)
                check_signature(:( $inner_signature::T where {T<:AbstractArmLeg}), inner_is_valid)
                check_signature(:( $inner_signature where {T} ),                   inner_is_valid)
                check_signature(:( $inner_signature where {T<:HelloWorld} ),       inner_is_valid)
            end
            function check_all_signatures(name, name_is_valid=true)
                check_outer_signature(:( $name() ),                       name_is_valid)
                check_outer_signature(:( $name(i) ),                      name_is_valid)
                check_outer_signature(:( $name(i=6) ),                    name_is_valid)
                check_outer_signature(:( $name(i::Int = 6) ),             name_is_valid)
                check_outer_signature(:( $name(; j) ),                    name_is_valid)
                check_outer_signature(:( $name(; j=7) ),                  name_is_valid)
                check_outer_signature(:( $name(; j::Int) ),               name_is_valid)
                check_outer_signature(:( $name(; j::Int = 7) ),           name_is_valid)
                check_outer_signature(:( $name(i; j) ),                   name_is_valid)
                check_outer_signature(:( $name(i; j=7) ),                 name_is_valid)
                check_outer_signature(:( $name(i; j::Int) ),              name_is_valid)
                check_outer_signature(:( $name(i; j::Int = 7) ),          name_is_valid)
                check_outer_signature(:( $name(i=6; j) ),                 name_is_valid)
                check_outer_signature(:( $name(i=6; j=7) ),               name_is_valid)
                check_outer_signature(:( $name(i=6; j::Int) ),            name_is_valid)
                check_outer_signature(:( $name(i=6; j::Int = 7) ),        name_is_valid)
                check_outer_signature(:( $name(i::Int; j) ),              name_is_valid)
                check_outer_signature(:( $name(i::Int; j=7) ),            name_is_valid)
                check_outer_signature(:( $name(i::Int; j::Int) ),         name_is_valid)
                check_outer_signature(:( $name(i::Int; j::Int = 7) ),     name_is_valid)
                check_outer_signature(:( $name(i::Int = 6; j) ),          name_is_valid)
                check_outer_signature(:( $name(i::Int = 6; j=7) ),        name_is_valid)
                check_outer_signature(:( $name(i::Int = 6; j::Int) ),     name_is_valid)
                check_outer_signature(:( $name(i::Int = 6; j::Int = 7) ), name_is_valid)
            end
            check_all_signatures(:f)
            check_all_signatures(:(Base.f))
            check_all_signatures(:(a + b), false)
            check_all_signatures(:(a()), false)
        end
        @testset "visit_exprs()" begin
            e = Expr(:root,
              Expr(:a,
                  1,
                  "2",
                  Expr(:b,
                      3,
                      Symbol("4")
                  )
              ),
              5,
              Expr(:c),
              "6"
            )
            paths = Vector{Vector{Int}}()
            exprs = Vector{Any}()
            visit_exprs(e) do path_indices, path
                push!(paths, copy(path_indices))
                push!(exprs, path[end])
            end
            @test paths == [
                [ ],
                [ 1 ],
                [ 1, 1 ],
                [ 1, 2 ],
                [ 1, 3 ],
                [ 1, 3, 1 ],
                [ 1, 3, 2 ],
                [ 2 ],
                [ 3 ],
                [ 4 ]
            ]
            @test exprs == [
                e,
                e.args[1],
                e.args[1].args[1], e.args[1].args[2],
                e.args[1].args[3], e.args[1].args[3].args[1], e.args[1].args[3].args[2],
                e.args[2], e.args[3], e.args[4]
            ]
        end
    end
end