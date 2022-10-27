#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Arrays}

An expression followed by a square-bracketed sequence of expressions
is parsed as an implicit use of the @rhombus(#{#%ref}) form, which is
normally bound to implement an array reference or assignment, as well
as other operations.

@dispatch_table(
  "array",
  @rhombus(Array),
  [arr.length(), Array.length(arr)]
)

@doc(
  fun Array(v :: Any, ...) :: Array
){

 Constructs a mutable array containing given arguments.

@examples(
  val a: Array(1, 2, 3),
  a,
  a[0],
  a[0] := 0,
  a
)

}

@doc(
  bind.macro 'Array($binding, ...)'
){

 Matches an array with as many elements as @rhombus(binding)s, where
 each element matches its corresponding @rhombus(binding).

@examples(
  val Array(1, x, y): Array(1, 2, 3),
  y
)

}

@doc(
  annot.macro 'Array',
  annot.macro 'Array.of($annotation)',
){

 Matches any array in the form without @rhombus(of). The @rhombus(of)
 variant matches an array whose elements satisfy @rhombus(annotation).

}

@doc(
  fun Array.make(length :: Integer, val = 0) :: Array
){

  Creates a fresh array with @rhombus(length) slots, where each slot
  is initialized to @rhombus(val).

@examples(
  Array.make(3, "x")
)

}

@doc(
  fun Array.length(arr :: Array) :: Integer,
){

 Returns the length of @rhombus(arr).

@examples(
  Array.make(3, "x").length()
)

}

