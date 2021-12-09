#lang scribble/rhombus/manual
@(import: "common.rhm"open)

@title{Multiple Values}

@doc[
  fun values(v, ...)
]{

 Returns the @rhombus[v]s as multiple result values.

 If only one @rhombus[v] is provided, the result is the same as just
 @rhombus[v]. Any other number of values must be received by a context
 that is expecting multiple values, such as with a
 @rhombus[values, ~bind] binding pattern.

}

@doc[
  bind.macro '(values($binding, ...))
]{

 Matches multiple result values corresponding to the number of
 @rhombus[binding]s, where each result matches the corresponing
 @rhombus[binding].

}
