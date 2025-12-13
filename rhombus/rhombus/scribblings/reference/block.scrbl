#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Block}

@doc(
  expr.macro 'block:
                $body
                ...'
  repet.macro 'block:
                 $repet
                 ...'
){

 Returns the result of the @rhombus(body) block, which may include local
 definitions in the case of an expression use of @rhombus(block) (but not
 a repetition use of @rhombus(block)). The @rhombus(body) is in @tail_position
 with respect to the @rhombus(block) form.

@examples(
  block:
    1
    2

  block:
    let one = 1
    one + one
)

}
