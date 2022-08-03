#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title{For-Meta Sequences}

@doc(
  defn.macro 'begin_for_meta:
                $body
                ...'
){

 The same as the @rhombus(body) sequence, but shifted to one phase
 greater. Defintions inside a @rhombus(begin_for_meta) block can be
 referenced in macro implements, for example.

@examples(
  ~eval: macro.make_for_meta_eval(),
  begin_for_meta:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'right_operand $(exp :: Arithmetic)':
    values(exp.y, ''),
  right_operand 1 + 2
)

}
