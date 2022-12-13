#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Dot}

@doc(
  expr.macro '$target . $identifier'
){

 Accesses a component of @rhombus(target), either statically or
 dynamically. The access is static when @rhombus(target) is a
 @tech{dot provider}.

 See also @rhombus(use_static).

@examples(
 [1, 2, 3].length(),
 class Posn(x, y),
 def p: Posn(1, 2),
 p.x
)

}
