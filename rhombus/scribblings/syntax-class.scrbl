#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "syntax-classes"){Syntax Classes}

As shown in @secref("syntax"), a variable can be bound in a syntax
pattern via @rhombus($), parentheses, @rhombus(::), and a
@deftech{syntax class} name to specify the kind of syntax the pattern
variable can match. The syntax classes @rhombus(Term, ~stxclass),
@rhombus(Group, ~stxclass), and @rhombus(Multi, ~stxclass) are built in,
among others.

@(rhombusblock:
    val '$(x :: Term)': '1'
)

Rhombus also supports user-defined syntax classes via
@rhombus(syntax.class). Use the @rhombus(syntax.class) form with a block
that contains @rhombus(pattern) with pattern alternatives:

@(rhombusblock:
    syntax.class Arithmetic:
     pattern
     | '$x + $y'
     | '$x - $y'
)

Equivalently, use a shorthand syntax that omits the use of
@rhombus(pattern) an inlines alternatives into the immediate
@rhombus(syntax.class) form:

@(rhombusblock:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y'
)

Defining a syntax class in this way makes it available for use in syntax
patterns, such as in @rhombus(match). The syntax class must be defined
at the same phase as the referencing pattern. To define a syntax class
for use in a macro definition, place it inside a
@rhombus(begin_for_meta) block.

@(rhombusblock:
    begin_for_meta:
      syntax.class Arithmetic
      | '$x + $y'
      | '$x - $y'
)

Once defined, a syntax class can be used to annotate a pattern
variable that matches any of pattern alternatives specified in the
syntax class. Generally, a syntax class can make a sequence of terms,
so a pattern variable annotated with a syntax class is bound to a
@tech{repetition} for use with @rhombus(...).

@(rhombusblock:
    expr.macro 'add_one_to_expr $(expr :: Arithmetic)':
      values('$expr ... + 1', '')

    add_one_to_expr 1 + 1 // expands to: 1 + 1 + 1
    add_one_to_expr 1 - 2 // expands to: 1 - 2 + 1
    add_one_to_expr 2 > 3 // error, "expected Arithmetic"
)

The @rhombus($)-escaped variables in a syntax class's patterns bind to
matched syntax objects as attributes of the class. They can be accessed
from a pattern variable using dot notation.

@(rhombusblock:
    expr.macro 'right_operand $(expr :: Arithmetic)':
      values(expr.y, '')

    right_operand 2 + 3 // expands to: 3
    right_operand 8 - 4 // expands to: 4
)

An attribute is accessible only when it appears in every pattern
alternative of a syntax class.

@(rhombusblock:
    syntax.class Arithmetic
    | '$x + $y + $z'
    | '$x - $y'

    val '$(expr :: Arithmetic)': '1 + 2 + 3' // matches successfully
    expr.y // expands to: '2'
    expr.z // error: attribute "z" not found
)

In other words, the attributes of a syntax class are defined by the intersection 
of all escaped pattern variables found in the pattern alternatives. 
