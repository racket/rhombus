#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Dot}

@doc(
  ~nonterminal:
    target_expr: block expr
    target_repet: block repet
  expr.macro '$target_expr . $id'
  expr.macro '$target_expr . $id $assign_op $expr'
  repet.macro '$target_repet . $id'
  grammar assign_op:
    :=
    $other_assign_op

){

 Accesses or updates a component of the value produce by
 @rhombus(target_expr), either statically
 or dynamically. The operation is static when @rhombus(target_expr) is a
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
