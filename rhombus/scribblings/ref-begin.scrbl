#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Begin}

@doc(
  decl.macro 'begin:
                $body
                ...'
){

 Returns the result of the @rhombus(body) block, which may include local
 definitions.

@examples(
  begin:
    1
    2,
  begin:
    val one: 1
    one + one
)

}

