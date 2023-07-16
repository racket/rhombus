#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Motivation}

Shrubbery notation is a vehicle for using a programming language in
much the same way that S-expression notation is a vehicle. Like
Racket's macro system as built on S-expressions, this proposal
introduces an expander layer that is built on shrubberies.

Here's an example of the kind of language that the expander is meant
to support:

@rhombusblock(
  import:
    weather

  class Posn(x, y)

  def home :: Posn:
    Posn(3, 7)

  fun abs(n): if n < 0 | -n | n

  fun manhattan_distance(p :: Posn):
    abs(home.x - p.x) + 5 * abs(home.y - p.y)

  fun another_manhattan_distance(Posn(x, y)):
    abs(home.x - x) + 5 * abs(home.y - y)

  fun
  | should_take_cab(Posn(0, 0)): #false
  | should_take_cab(p :: Posn):
      manhattan_distance(p) > 10 || weather.currently_raining()
  | should_take_cab(#false): #false
)

The intent here is that @rhombus(val) and @rhombus(fun) are
macro-implemented and recognize various forms of definitions, including
simple binding, functions, and functions that have pattern-matching
cases. The @rhombus(val) and @rhombus(fun) forms are not meant to
know about @rhombus(::) specifically; the @rhombus(::) is meant to be a
binding operator that checks whether the value flowing to the binding
satisfies a predicate, and it may also associate compile-time
information to a binding, such as the information that @rhombus(p) is a
@rhombus(Posn) instance. The name @rhombus(Posn) works in an expression
to construct a position value, while in a binding position,
@rhombus(Posn) works to construct a pattern-matching binding (again,
without @rhombus(val) or @rhombus(fun) knowing anything specific about
@rhombus(Posn)). Meanwhile, @rhombus(.), @rhombus(-), @rhombus(+),
@rhombus(*), @rhombus(<), and @rhombus(||) are the obvious operators
with the usual precedence. Unlike the other operators, the @rhombus(.)
operator's right-hand side is not an expression; it must always be an
identifier. The @rhombus(weather.currently_raining) form looks like a
use of the @rhombus(.) operator, but it's meant here to be a use of the
namer @rhombus(weather) that is bound by @rhombus(import) and recognizes
@rhombus(.) to access the imported @rhombus(currently_raining) binding,
which might be a macro instead of a variable that is bound to a
function.

The Rhombus expander is further meant to a support a language where
new operators can be defined in a function-like way, like this:

@rhombusblock(
  operator (x <> y):
    Posn(x, y)

  1 <> 2 // same as Posn(1, 2)
)

Alternatively, operators can be defined in a more general, macro-like
way. For example, defining @rhombus(->) as an alias for @rhombus(.)
requires a macro, since the right-hand side of @rhombus(.) is not an
expression. Using @rhombus('') for ``quote'' and @rhombus($) for ``unquote,''
the @rhombus(->) operator might be implemented in a pattern-matching
macro as

@rhombusblock(
  expr.macro '$x -> $y $tail ...':
    values('$x . $y', '$tail ...')

  home->x + 1 // same as home.x + 1
)

The intent here is that @rhombus(expr.macro) (the @rhombus(.) there is
like using an import, accessing the @rhombus(macro) form within an
@rhombus(expr) group of bindings) allows a macro to consume as many
terms after the operator as it wants, and the macro must return two
values: a quoted expression for the expansion plus leftover terms for
further expression parsing (i.e., @rhombus(tail) in the example use will
hold @rhombus(+ 1)). Macros can work for other contexts, too, such as
binding positions. Here's a definition that extends the @rhombus(<>)
operator to make it work in binding positions:

@rhombusblock(
  bind.macro '$x <> $y $tail ...':
    values('Posn($x, $y)', '$tail ...')

  // uses <> both for argument binding and result expression:
  fun flip(x <> y):
    y <> x

  flip(1 <> 2) // produces 2 <> 1
)

The above examples run in @Rhombus, but the examples are meant only to
illustrate some of the ingredients that the expander supports. This
proposal does not intended to specify a language with specific
definition forms, expression forms, and so on.

To define a new expander layer for shrubbery notation, we take Racket's
primitives as given: syntax objects, scopes, modules, and more. We also
recycle some variant of Racket as the target for shrubbery expansion. In
principle, the variant could be minimal, corresponding to the core forms
that all Racket modules expand into, but some larger variant (including
keyword arguments, for example) is likely a better choice of
interoperability with Racket modules. The enforestation and expansion
process here are defined in terms of the S-expression form of parsed
shrubbery notation (really, syntax-object form, so it can include scopes
to determine a mapping for identifiers and operators). The @rhombus(<>)
and @rhombus(->) examples above use operator- and macro-definition forms
in terms of shrubbery notation, but this proposal focused on the
lower-level mechanisms that allow such shrubbery-native forms to be
implemented.

Since shrubbery notation is intended to be used with less grouping than
is normally present in S-expressions, a key goal of this proposal is to
specify an extensible traversal of shrubberies to turn them into Racket
terms. In particular, the proposal addresses the problem of parsing a
mixture of operators and other terms within a shrubbery group, allowing
the definition of infix, prefix, and postfix operators for expressions,
bindings, and other contexts. The handling of operators is directly
based on Honu, but with several refinements—including a framework for
relative and non-transitive operator precedence that is more like
Fortress than Honu.

Although this proposal does not specify definition forms, we assume some
way of mapping an identifier or operator to a compile-time entity, such
as a macro transformer. (Throughout this proposal, we use ``mapping'' to
refer to this binding in the sense of @racket_define_syntax, as
opposed to binding positions in a language that is built with the
Rhombus expander.) We also assume that mappings can be local, so that
delaying the expansion of nested groups is useful while compile-time
mappings are created by surrounding terms.
