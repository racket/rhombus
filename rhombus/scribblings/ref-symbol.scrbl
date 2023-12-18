#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

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
  expr.macro '#' $id'
  expr.macro '#' $keyword'
  repet.macro '#' $id'
  repet.macro '#' $keyword'
  bind.macro '#' $id'
  bind.macro '#' $keyword'
){

 As an expression, repetition, or binding, @rhombus(#') produces or matches a symbol
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

@doc(
  fun Symbol.from_string(str :: ReadableString)
    :: Symbol
  fun Symbol.uninterned_from_string(str :: ReadableString)
    :: Symbol
  fun Symbol.unreadable_from_string(str :: ReadableString)
    :: Symbol
){

 Converts a string to a symbol with the same character content. An
 @deftech{uninterned} symbol is not equal to any other symbol, even ones
 with the same character content. An @deftech{unreadable} symbol is only
 equal to other unreadable symbols with the same character content.

@examples(
  Symbol.from_string("apple")
  Symbol.from_string("apple") == #'apple
  Symbol.uninterned_from_string("apple")
  Symbol.uninterned_from_string("apple")
    == Symbol.uninterned_from_string("apple")
  Symbol.unreadable_from_string("apple")
  Symbol.unreadable_from_string("apple") == #'apple
  Symbol.unreadable_from_string("apple")
    == Symbol.unreadable_from_string("apple")
)

}


@doc(
  fun Symbol.gen() :: Symbol
  fun Symbol.gen(name :: ReadableString || Symbol) :: Symbol
){

 Produces an @tech{uninterned} symbol with a character content optionally derived
 from @rhombus(name) (typically with digits appended as a debugging aid).

}
