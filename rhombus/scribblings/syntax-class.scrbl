#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title[~tag: "syntax-classes"]{Syntax Classes}

In a Rhombus syntax form, pattern variables escaped with @rhombus[$] and 
surrounded by parenthesescan be annotated with a syntax class name using 
@rhombus[::] to specify the kind of syntax the pattern variable can match on. 
Rhombus has several built-in syntax classes such as @rhombus[Term], 
@rhombus[Group], and @rhombus[Multi].

@(rhombusblock:
    val '$(x :: Term)': '1'
)

Rhombus also supports user-defined syntax classes that can annotate pattern 
variables in the same way. Custom syntax classes are useful for defining 
resusable patterns that can vary. To define a syntax class, use the 
@rhombus[stx_class] special form with a block that contains the form 
@rhombus[pattern] followed by alternatives. 

@(rhombusblock:
    stx_class Arithmetic:
        pattern
        | '$x + $y'
        | '$x - $y'
)

Or, use a shorthand syntax that omits the use of @rhombus[pattern].

@(rhombusblock:
    stx_class Arithmetic
    | '$x + $y'
    | '$x - $y'
)

To use a syntax class definion for a macro, place it inside a 
@rhombus[begin_for_meta] block.

@(rhombusblock:
    begin_for_meta:
        stx_class Arithmetic
        | '$x + $y'
        | '$x - $y'
)

The patterns of a syntax class are defined with quasiquoted syntax objects (@rhombus[''])
and can include any literal syntax. Once defined, a custom syntax 
class can be used to annotate a pattern variable that will match on the shape 
of one of the specified pattern alternatives.

@(rhombusblock:
    expr.macro 'add_one_to_expr $(expr :: Arithmetic)':
        values('$expr ... + 1', '')
    
    add_one_to_expr 1 + 1 // expands to: 1 + 1 + 1
    add_one_to_expr 1 - 2 // expands to: 1 - 2 + 1
    add_one_to_expr 2 > 3 // error, "expected Arithmetic"
)

The @rhombus[$]-escaped variables in a syntax class's patterns will bind to 
matched syntax as attributes of the class. They can be accessed from a pattern 
variable using dot-notation. 

@(rhombusblock:
    expr.macro 'right_operand $(expr :: Arithmetic)':
        values(expr.y, '')
    
    right_operand 2 + 3 // expands to: 3
    right_operand 8 - 4 // expands to: 4
)

Attributes of a syntax class must appear in every pattern alternative in order 
to be referred to with dot-notation.

@(rhombusblock:
    stx_class Arithmetic
    | '$x + $y + $z'
    | '$x - $y'

    val '$(expr :: Arithmetic)': '1 + 2 + 3' // matches successfully
    expr.y // expands to: '2'
    expr.z // error: attribute "z" not found
)

In other words, the attributes of a syntax class are defined by the intersection 
of all escaped pattern variables found in the pattern alternatives. 
