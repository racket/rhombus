#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Dot}

@doc(
  expr.macro '$target . $identifier',
  expr.macro '$target . $identifier := $expression'
){

 Accesses or updates a component of @rhombus(target), either statically or
 dynamically. The operation is static when @rhombus(target) is a
 @tech{dot provider}.

 See also @rhombus(use_static).

@examples(
 [1, 2, 3].length(),
 class Posn(x, mutable y),
 def p: Posn(1, 2),
 p.x,
 p.y := 20,
 p
)

}
