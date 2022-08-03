#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Booleans}

@doc(
  annotation.macro 'Boolean'
){

  Matches @rhombus(#true) or @rhombus(#false)

}

@doc(
  expr.macro '$expr || $expr'
){

 Produces the value of the first @rhombus(expr) if it is
 non-@rhombus(#false), otherwise produces the value(s) of the second
 @rhombus(expr).

 The second @rhombus(expr) is evaluated in tail position with respect to
 the @rhombus(||) form.

}

@doc(
  expr.macro '$expr && $expr'
){

 Produces @rhombus(#false) if the the value of the first @rhombus(expr)
 is @rhombus(#false), otherwise produces the value(s) of the second
 @rhombus(expr).

 The second @rhombus(expr) is evaluated in tail position with respect to
 the @rhombus(&&) form.

}

@doc(
  operator (! v):: Boolean
){

 Returns @rhombus(#true) if @rhombus(v) is @rhombus(#false),
 @rhombus(#false) otherwise.


@examples(
  !#false,
  !#true,
  !"false"
)

}
