#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Keywords}

A @deftech{keyword} by itself does not work as an expression, but
keywords exist as values, and the @rhombus(#') operator can
produce a keyword value. Keywords are always interned, and they are
equal by @rhombus(==) only when they are equal by @rhombus(===).

See also @rhombus(#'), which works for keywords as well as symbols.

@doc(
  annot.macro 'Keyword'
){

  Matches keyword values.

}


@doc(
  fun Keyword.from_string(str :: ReadableString) :: Keyword
  fun Keyword.from_symbol(sym :: Symbol) :: Keyword
){

 Converts a string or symbol to a keyword with the same character
 content, not counting a keyword's leading @litchar{~}.

@examples(
  Keyword.from_string("apple")
  Keyword.from_symbol(#'apple)
)

}
