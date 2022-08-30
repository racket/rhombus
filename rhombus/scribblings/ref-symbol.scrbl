#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Symbols}

A @deftech{symbol} value reflects internal representation of
@tech{syntax-object} identifier content, without the binding or
source-location information that is associated with an identifier. A
symbol is similar to a string, but symbols are typically interned and
they are equal by @rhombus(==) only when they are equal by
@rhombus(===).

@doc(
  annotation.macro 'Symbol'
){

  Matches symbols.

}

@doc(
  expr.macro '«symbol'$identifier'»',
  bind.macro '«symbol'$identifier'»',
  expr.macro 'symbol($identifier)',
  bind.macro 'symbol($identifier)'
){

 Produces or matches a symbol whose characters are the same as
 @rhombus(identifier). Parentheses and quotes are interchangeable.

@examples(
  symbol'hello',
  symbol'hello' +& " there"
)

}
