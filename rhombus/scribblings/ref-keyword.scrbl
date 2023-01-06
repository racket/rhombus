#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Keywords}

A @deftech{keyword} by itself does not work as an expression, but
keywords exist as values, and the @rhombus(#') operator can
produce a keyword value. Keywords are always interned, and they are
equal by @rhombus(==) only when they are equal by @rhombus(===).

@doc(
  annot.macro 'Keyword'
){

  Matches keyword values.

}

}
