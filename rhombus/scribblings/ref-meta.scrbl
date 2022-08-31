#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title{Meta Definitions and Expressions}

@doc(
  decl.macro 'meta:
                $body
                ...'
){

 The same as the @rhombus(body) sequence, but shifted to one phase
 greater. Defintions inside a @rhombus(meta) block can be
 referenced in macro implementations, for example.

 See also the @rhombus(meta, ~impmod) import modifier.

@examples(
  ~eval: macro.make_for_meta_eval(),
  meta:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'right_operand $(exp :: Arithmetic)':
    values(exp.y, ''),
  right_operand 1 + 2
)

}
