#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Strings}

@doc[
  annotation.macro 'String'
]{

  Matches strings.

}

@doc[
  fun str(lst :: List) :: String
]{

 Coerces all of the elements of @rhombus[lst] to strings, then appends the strings.

 The string for of a value is the same way that @rhombus[display] would
 print it, which means that strings, symbols, and keywords print as their
 character content.

@examples[
  str(["hello", "world"]),
  str(["it goes to ", 11]),
  str(["the list ", [1, 2, 3], " has ", 3, " elements"]),
  @str{the list @([1, 2, 3]) has @3 elements}
]

}
