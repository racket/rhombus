#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def sc_eval = make_rhombus_eval())

@examples(
  ~eval: sc_eval,
  ~hidden: #true,
  import:
    rhombus/meta open,
  class Posn(x, y)
)

@title(~tag: "syntax-classes"){Syntax Classes}

As shown in @secref("syntax"), a variable can be bound in a syntax
pattern via @rhombus($), parentheses, @rhombus(::), and a
@deftech{syntax class} name to specify the kind of syntax the pattern
variable can match. The syntax classes @rhombus(Term, ~stxclass),
@rhombus(Group, ~stxclass), and @rhombus(Multi, ~stxclass) are built in,
among others.

@(demo:
    ~defn:
      def '$(x :: Term)' = '1'
)

Rhombus also supports user-defined syntax classes via
@rhombus(syntax.class). Use the @rhombus(syntax.class) form with a block
that contains @rhombus(~pattern) with pattern alternatives:

@(demo:
    ~defn:
      syntax.class Arithmetic:
       ~pattern
       | '$x + $y'
       | '$x - $y'
)

Equivalently, use a shorthand syntax that omits the use of
@rhombus(~pattern) and inlines alternatives into the immediate
@rhombus(syntax.class) form:

@(demo:
    ~defn:
      syntax.class Arithmetic
      | '$x + $y'
      | '$x - $y'
)

Defining a syntax class in this way makes it available for use in syntax
patterns, such as in @rhombus(match). The syntax class must be defined
at the same phase as the referencing pattern. To define a syntax class
for use in a macro definition, place it inside a
@rhombus(meta) block.

@(demo:
    ~eval: sc_eval
    ~defn:
      meta:
        syntax.class Arithmetic
        | '$x + $y'
        | '$x - $y'
)

Once defined, a syntax class can be used to annotate a pattern
variable that matches any of pattern alternatives specified in the
syntax class. Generally, a syntax class can make a sequence of terms,
so a pattern variable annotated with a syntax class is bound to a
@tech{repetition} for use with @rhombus(...).

@(demo:
    ~eval: sc_eval
    ~defn:    
      expr.macro 'add_one_to_expr $(expr :: Arithmetic)':
        values('$expr ... + 1', '')
    ~repl:
      add_one_to_expr 1 + 1
      add_one_to_expr 1 - 2
      ~error: add_one_to_expr 2 > 3
)

The @rhombus($)-escaped variables in a syntax class's patterns bind to
matched syntax objects as attributes of the class. They can be accessed
from a pattern variable using dot notation.

@(demo:
    ~eval: sc_eval
    ~defn:
      expr.macro 'right_operand $(expr :: Arithmetic)':
        values(expr.y, '')
    ~eval:
      right_operand 2 + 3
      right_operand 8 - 4
)

An attribute is accessible only when it appears in every pattern
alternative of a syntax class.

@(demo:
    ~eval: sc_eval
    ~defn:
      syntax.class Arithmetic
      | '$x + $y + $z'
      | '$x - $y'
    ~repl:
      def '$(expr :: Arithmetic)' = '1 + 2 + 3'
      expr.y
      ~error: expr.z
)

In other words, the attributes of a syntax class are defined by the intersection 
of all escaped pattern variables found in the pattern alternatives. 
