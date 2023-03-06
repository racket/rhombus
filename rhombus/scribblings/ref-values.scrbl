#lang scribble/rhombus/manual
@(import: "common.rhm"open
          "nonterminal.rhm" open)

@title{Multiple Values}

@doc(
  fun values(v, ...)
){

 Returns the @rhombus(v)s as multiple result values.

 If only one @rhombus(v) is provided, the result is the same as just
 @rhombus(v). Any other number of values must be received by a context
 that is expecting multiple values, such as with a
 @rhombus(values, ~bind) binding pattern.

}

@doc(
  bind.macro 'values($bind, ...)'
){

 Matches multiple result values corresponding to the number of
 @rhombus(bind)s, where each result matches the corresponing
 @rhombus(bind).

}

@doc(
  reducer.macro 'values($id = $expr, ...)'
){

 A @tech{reducer} used with @rhombus(for), expects as many results from a
 @rhombus(for) body as @rhombus(id)s. For the first iteration of
 the @rhombus(for) body, each @rhombus(id)'s value is the result
 of the corresponding @rhombus(expr). The results of a @rhombus(for) body
 for one iteration then serve as the values of the @rhombus(id)s
 for the next iteration. The values of the whole @rhombus(for) expression
 are the final values of the @rhombus(id)s.

}
