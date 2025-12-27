#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Void}

The @deftech{void} literal is written as @rhombus(#void). It is intended
to represent the absence of a useful value, and it is more often used as
the result of an operation that performs a mutating effect.

Returning @tech(~doc: model_doc, ~key: "multiple value"){zero values}
could also represent the absence of a useful result value, but
@rhombus(#void) is more conventional, because the possibility of
multiple result values can be sometimes inconvenient.

@doc(
  annot.macro 'Void'
){

  Matches @rhombus(#void).

}
