#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Symbols}

A @deftech{symbol} value reflects internal representation of
@tech{syntax-object} identifier content, without the binding or
source-location information that is associated with an identifier. A
symbol is similar to a string, but symbols are typically interned and
they are equal by @rhombus(==) only when they are equal by
@rhombus(===). The @rhombus(#') operator can produce a symbol value.

@doc(
  annot.macro 'Symbol'
){

  Matches symbols.

}

@doc(
  expr.macro '#' $identifier'
  expr.macro '#' $keyword'
  bind.macro '#' $identier'
  bind.macro '#' $keyword'
){

 As an expression or binding, @rhombus(#') produces or matches a symbol
 or keyword, depending whether @rhombus(#') is followed by an identifier
 or keyword.

@examples(
  ~repl:
    #'hello
    #'hello +& " there"
    #'~skeleton
  ~repl:
    match #'goodbye
    | #'hello: "hi"
    | #'goodbye: "bye"
)

}
