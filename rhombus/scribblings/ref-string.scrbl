#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Strings}

A @deftech{string} is a sequence of Unicode characters. A string works
with map-referencing @brackets to access a character via
@rhombus(#%ref). A string also works with the @rhombus(++) operator to
append strings, but a @rhombus(+&) can be used to append strings with
the static guaratee that the result is a string. A string can be used as
@tech{sequence}, in which case it supplies its bytes in order.


@dispatch_table(
  "string"
  @rhombus(String)
  [str.length(), String.length(str)]
  [str.to_int(), String.to_int(str)]
  [str.to_number(), String.to_number(str)]
)

@doc(
  annot.macro 'String'
){

  Matches strings.

}

@doc(
  fun to_string(v) :: String
){

 Coerces @rhombus(v)  to a string.

 The string for of a value corresponds to the way that @rhombus(display)
 would print it, which means that strings, symbols, identifiers, and
 keywords convert as their character content.

@examples(
  to_string(10)
  to_string('hello')
  to_string([1, 2, 3])
)

}


@doc(
  operator (v1 +& v2) :: String
){

 Coerces @rhombus(v1) and @rhombus(v2) to a string, then appends the strings.

 The value is coerced to a string in the same way as by
 @rhombus(to_string).

@examples(
  "hello" +& "world"
  "it goes to " +& 11
  "the list " +& [1, 2, 3] +& " has " +& 3 +& " elements"
)

}


@doc(
  fun String.length(str :: String) : NonnegInt
){

 Returns the number of characters in @rhombus(str).

@examples(
  String.length("hello")
  "hello".length()
)

}


@doc(
  fun String.to_int(str :: String) : Optional[Int]
){

 Parses @rhombus(str) as an integer, returning @rhombus(#false) if the
 string does not parse as an integer, otherwise returning the integer
 value.

@examples(
  String.to_int("-42")
  String.to_int("42.0")
  String.to_int("fourty-two")
  "100".to_int()
)

}


@doc(
  fun String.to_number(str :: String) : Optional[Number]
){

 Parses @rhombus(str) as a number, returning @rhombus(#false) if the
 string does not parse as a number, otherwise returning the number
 value.

@examples(
  String.to_number("-42")
  String.to_number("42.0")
  String.to_number("fourty-two")
  "3/4".to_number()
)

}