#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@title{Dot}

@doc(
  ~nonterminal:
    target_expr: begin expr
    target_repet: begin repet                 
  expr.macro '$target_expr . $id'
  expr.macro '$target_expr . $id := $expr'
  repet.macro '$target_repet . $id'
){

 Accesses or updates a component of @rhombus(target), either statically
 or dynamically. The operation is static when @rhombus(target) is a
 @tech{dot provider}. The access form also works as a @tech{repetition}
 given a repetition for the target.

 See also @rhombus(use_static).

@examples(
  [1, 2, 3].length()
  class Posn(x, mutable y)
  def p: Posn(1, 2)
  p.x
  p.y := 20
  p
)

}
