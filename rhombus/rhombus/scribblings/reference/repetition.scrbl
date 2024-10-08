#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Repetitions}

A @deftech{repetition} represents a sequence of values, and it can be used
in designated repetition positions (in much the same way that
expressions appear in expression positions and bindings in binding
positions). For example, a repetition can be used at the end of a list
with @rhombus(...) after it.

One way to create a repetition is through a binding that uses @(dots).
Some expression-like forms are also repetition forms, creating new
repetitions from one or more repetitions. For example, if @rhombus(x) is
bound as a repetition, then @rhombus(x+1) can be used as a repetition to
add @rhombus(1) to each element of @rhombus(x):

@examples(
  def [x, ...] = [1, 2, 3]
  [x+1, ...]
)

When @rhombus(+) is used as a repetition operator, then it expects a
repetition for both arguments, but a literal value like @rhombus(1)
works as a repetition via @rhombus(#%literal).

A repetition has a @italic{depth}, and each repetition context expects a
repetition of a particular depth, typically based on how many
@(dots_expr)s appear after the repetition context. In the above example,
@rhombus(x) is bound as a repetition of depth 1, and it is used in a
context of depth 1, since there is one @dots_expr after @rhombus(x+1) to
form the list. Using multiple @dots in a binding context typically
binds at a greater depth, as in this example that binds and uses
@rhombus(z) at depth 2:

@examples(
  def [[z, ...], ...] = [[1, 2, 3], [4, 5]]
  [[z+1, ...], ...]
)

More precisely, in this example, the outer list construction expects a
repetition of depth 1 before its @dots_expr, and @rhombus([z+1, ...])
creates a repetition of depth 1. The @rhombus([z+1, ...]) repetition is
depth 1, because a list repetition subtracts one from the depth of a
repetition before (another) @dots_expr, and @rhombus(z+1) has depth 2
due to the @rhombus(z) binding.

When a repetition is followed by multiple @rhombus(...)s in a row, as
opposed to nested @rhombus(...)s, then the repetitions that would be
accessed by nesting are flattened into a single repetition. This
flattening has the effect of appending sequences.

@examples(
  def [[z, ...], ...] = [[1, 2, 3], [4, 5]]
  [z, ..., ...]
)

When a repetition form combines multiple repetitions, then unless
documented otherwise, elements at the same repetition depth are drawn
from the repetitions in parallel.

@examples(
  def [x, ...] = [1, 2, 3]
  def [y, ...] = ["a", "b", "c"]
  [[x, y], ...]
)

When combined repetitions are at different depths, the shallower
repetition is repeated for outer layers of the deeper repetition. That's
why the @rhombus(x+1) and @rhombus(z+1) examples above work: a literal
@rhombus(1) works as repetition of depth 0, but it is repeated as needed
to match the @rhombus(x) repetition of depth 1 or the @rhombus(z)
repetition of depth 2. A repetition of depth 2 can be similarly repeated
to match a repetition of depth 2:

@examples(
  def [[z, ...], ...] = [[1, 2, 3], [4, 5, 6]]
  def [y, ...] = [10, 100, 1000]
  [[z+y, ...], ...]
)

In other words, unless otherwise documented, the depth of a repetition
formed by combining repetitions is the maximum of the depths of the
combined repetitions, so @rhombus(z+y) is a repetition of depth 2.

Expressions with side effects or short-circuiting operators can appear
within a repetition. Each effect and control-flow choice is applied on
demand per element of the repetition, which can be different than
constructing an intermediate repetition.

@examples(
  def [x, ...] = [1, 2, 3]
  [x > 1 && println(x), ...]
  block:
    let [void, ...] = [println(x), ...]
    [x > 1 && void, ...]
)

When an identifier is bound as a repetition, it is bound in the
@rhombus(repet, ~space) space, but also in the @rhombus(expr, ~space).
The @rhombus(expr, ~space) binding reports an error, but the intent of
the binding is to shadow any existing expression binding for the identifier.

@doc(
  bind.macro '...',
  expr.macro '...'
){

The @dots ``binding'' form or @dots_expr
``expression'' form is not really allowed as an binding or expression,
but it can appear in places where an binding or expression would
otherwise be allowed.

In a binding-like position, @dots tends to change a
nearby binding into a @tech{repetition} binding. For example, in
@rhombus(fun (#,(@rhombus(x, ~var)), #,(dots)): #,(@rhombus(body, ~var))),
the @dots causes @rhombus(x, ~var) to be bound to a
repetition.

In an expression-like position, @dots tends to change a
nearby expression position into a @tech{repetition} position, which is
a place where a repetition binding or operator can be used. For
example, the list expression @rhombus([#,(@rhombus(x, ~var)), #,(dots_expr)])
has @rhombus(x, ~var) in a repetition position, which would make sense
as part of the @rhombus(body, ~var) of
@rhombus(fun (#,(@rhombus(x, ~var)), #,(dots)): #,(@rhombus(body, ~var))),
since @rhombus(x, ~var) is bound there as a repetition.

Function arguments, list patterns, and syntax patterns are among the
places that recognize @dots to create repetition
bindings. Function calls, list constructs, and syntax templates are
among the places that recognize @dots to use repetition
positions.

}

@doc(
  ~also_meta
  ~nonterminal:
    list_expr: block expr
    list_bind: def bind ~defn
  expr.macro '& $list_expr'
  bind.macro '& $list_bind'
){

 The @rhombus(&) expression operator and binding operator can only be
 used in places where it's specifically recognized, normally either to
 reference or bind the ``rest'' of a data structure. The @rhombus(List)
 constructor, @rhombus(Map) constructor, @rhombus(fun) form, and the
 @rhombus(#%call) form are among the places that recognize
 @rhombus(&).

@examples(
  def [a, b, & others] = [1, 2, 3, 4]
  others
  [0, & others]
)

}

@doc(
  ~also_meta
  ~nonterminal:
    map_expr: block expr
    map_bind: def bind ~defn
  expr.macro '~& $map_expr'
  bind.macro '~& $map_bind'
){

 The @rhombus(~&) expression operator and binding operator can only be
 used in places where it's specifically recognized, normally to bind the
 ``rest'' of a map with keywords as keys. The @rhombus(fun) and
 @rhombus(#%call) forms are among the places that recognize
 @rhombus(~&).

@examples(
  fun roster(~manager: who, ~& players):
    players
  roster(~pitcher: "Dave", ~manager: "Phil", ~catcher: "Johnny")
)

}
