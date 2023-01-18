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
  syntax_binding.macro '#' $identifier'
){

 As an expression or binding, @rhombus(#') produces or matches a symbol
 or keyword, depending whether @rhombus(#') is followed by an identifier
 or keyword.

 The @rhombus(#') operator also works in a syntax binding context (i.e.,
 within @rhombus($, ~bind)). In that case, it must be followed by an
 identifier, and it matches an identifier that has the same symbol (and
 not necessarily the same binding) as the one in the pattern.

@examples(
  ~repl:
    #'hello
    #'hello +& " there"
    #'~skeleton
  ~repl:
    match #'goodbye
    | #'hello: "hi"
    | #'goodbye: "bye"
  ~repl:
    def unbound_map_id = Syntax.make(#'Map)
    match unbound_map_id
    | 'Map': "map"
    | ~else: "other"
    match unbound_map_id
    | '$(#'Map)': "map"
    | ~else: "other"
)

}
