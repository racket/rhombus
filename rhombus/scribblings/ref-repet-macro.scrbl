#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Repetition Macros}

@doc(
  space.transform repet
){

 The @tech{space} for bindings of @tech{repetitions}.

}

@doc(
  defn.macro 'repet.macro ....'
){

 This form is not yet available, but it will bind in the
 @rhombus(repet, ~space) @tech{space} (and not the @rhombus(expr, ~space)
 space).

}
