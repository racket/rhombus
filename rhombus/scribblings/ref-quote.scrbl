#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Quoting Data}

@doc(
  expr.macro '#' $identifier'
  expr.macro '#' $keyword'
  bind.macro '#' $identier'
  bind.macro '#' $keyword'
){

 Produces or matches a symbol or keyword, depending whether
 @rhombus(#') is followed by an identifier or keyword.

@examples(
  #'hello
  #'hello +& " there"
  #'~skeleton
)

}
