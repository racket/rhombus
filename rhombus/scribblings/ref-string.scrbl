#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Strings}


@doc(
  annotation.macro 'String'
){

  Matches strings.

}

@doc(
  operator (v1 +& v2) :: String
){

 Coerces @rhombus(v1) and @rhombus(v2) to a string, then appends the strings.

 The string for of a value is the same way that @rhombus(display) would
 print it, which means that strings, symbols, and keywords print as their
 character content.

@examples(
  "hello" +& "world",
  "it goes to " +& 11,
  "the list " +& [1, 2, 3] +& " has " +& 3 +& " elements"
)

}
