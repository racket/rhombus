#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Begin}

@doc(
  expr.macro 'begin:
                $body
                ...'
){

 Returns the result of the @rhombus(body) block, which may include local
 definitions.

@examples(
  begin:
    1
    2

  begin:
    def one: 1
    one + one
)

}

