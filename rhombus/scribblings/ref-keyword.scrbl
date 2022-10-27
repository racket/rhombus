#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Keywords}

A @deftech{keyword} by itself does not work as an expression, but
keywords exist as values, and the @rhombus(keyword) expression form
produces a keyword value. Keywords are always interned, and they are
equal by @rhombus(==) only when they are equal by @rhombus(===).

@doc(
  annot.macro 'Keyword'
){

  Matches keyword values.

}

@doc(
  expr.macro '«keyword'$a_keyword'»',
  bind.macro '«keyword'$a_keyword'»',
  expr.macro 'keyword($a_keyword)',
  bind.macro 'keyword($a_keyword)'
){

 Produces or matches a keyword that is the same as
 @rhombus(a_keyword). Parentheses and quotes are interchangeable.

@examples(
  keyword'~hello',
  keyword'~hello' +& " there"
)

}
