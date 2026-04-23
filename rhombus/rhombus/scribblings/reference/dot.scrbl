#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/memory)

@title{Dot}

@doc(
  ~nonterminal:
    target_expr: block expr
    target_repet: block repet
    old_expr: block expr
  expr.macro '$target_expr . $id'
  expr.macro '$target_expr . $id $assign_op $expr'
  repet.macro '$target_repet . $id'
  operator_order:
    ~order: member_access
  grammar assign_op
  | :=
  | := ~cas $old_expr ~to
  | $other_assign_op

){

 Accesses or updates a component of the value produced by
 @rhombus(target_expr), either statically
 or dynamically. The operation is static when @rhombus(target_expr) is a
 @tech(~doc: guide_doc){dot provider}. The access form also works as a @tech{repetition}
 given a repetition for the target.

 See also @rhombus(use_static).

@examples(
  [1, 2, 3].length()
  class Posn(x, mutable y)
  def p = Posn(1, 2)
  p.x
  p.y := 20
  p
)

 The @rhombus(:= ~cas old_expr ~to) assignment form is limited to a
 @rhombus(target_expr.id) that statically refers to a mutable field of an
 object. It updates the field only when the current field value is
 @rhombus(===) to the result of @rhombus(old_expr) and only if the value
 can be atomically replaced with the result of @rhombus(expr). Otherwise,
 the current value is left intact. In this mode, the assignment
 expression produces a @rhombus(Boolean, ~annot) result: @rhombus(#true)
 if the value was replaced, and @rhombus(#false) if not. Beware that on
 some platforms, a ``spurious'' failure can produce a @rhombus(#false)
 result and unchanged content even when the current content matches the
 result of @rhombus(old_expr). See also @rhombus(memory.order_acquire)
 and @rhombus(memory.order_release).

}
