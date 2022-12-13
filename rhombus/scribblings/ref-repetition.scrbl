#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title{Repetitions}

A @deftech{repetition} represents a sequence of values and can be used
in designated repetition positions (in much the same way that
expressions appear in expression positions and bindings in binding
positions). For example, a repetition can be used at the end of a list
with @rhombus(...) after it.

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
@rhombus(fun($$(@rhombus(x, ~var)), $$(dots)): $$(@rhombus(body, ~var))),
the @dots causes @rhombus(x, ~var) to be bound to a
repetition.

In an expression-like position, @dots tends to change a
nearby expression position into a @tech{repetition} position, which is
a place where a repetition binding or operator can be used. For
example, the list expression @rhombus([$$(@rhombus(x, ~var)), $$(dots_expr)])
has @rhombus(x, ~var) in a repetition position, which would make sense
as part of the @rhombus(body, ~var) of
@rhombus(fun($$(@rhombus(x, ~var)), $$(dots)): $$(@rhombus(body, ~var))),
since @rhombus(x, ~var) is bound there as a repetition.

Function arguments, list patterns, and syntax patterns are among the
places that recognize @dots to create repetition
bindings. Function calls, list constructs, and syntax templates are
among the places that recognize @dots to use repetition
positions.

}

@doc(
  expr.macro '& list_expr',
  bind.macro '& list_binding'
){

 The @rhombus(&) expression operator and binding operator can only be
 used in places where its specifically recognized, normally either to
 reference or bind the ``rest'' of a data structure. The @rhombus(List)
 constructor, @rhombus(Map) constructor, @rhombus(fun) form, and the
 @rhombus(#{#%call}) form are among the places that recognize
 @rhombus(&).

@examples(
  def [a, b, &others]: [1, 2, 3, 4],
  others,
  [0, &others]
)

}

@doc(
  expr.macro '~& map_expr',
  bind.macro '~& map_binding'
){

 The @rhombus(~&) expression operator and binding operator can only be
 used in places where its specifically recognized, normally to bind the
 ``rest'' of a map with keywords as keys. The @rhombus(fun) and
 @rhombus(#{#%call}) forms are among the places that recognize
 @rhombus(~&).

@examples(
  fun roster(~manager: who, ~&players):
    players,
  roster(~pitcher: "Dave", ~manager: "Phil", ~catcher: "Johnny")
)

}
