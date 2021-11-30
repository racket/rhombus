#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Arrays}

An array works with map-referencing square brackets to access a list
element by position (in constant time), and it works with square
brackets in combination with the assignment operator @rhombus[:=] to
update the array.

@doc[
  fun Array(v :: Any, ...) :: Array
]{

 Constructs a mutable array containing given arguments.

@examples[
  val a: Array(1, 2, 3),
  a,
  a[0],
  a[0] := 0,
  a
]

}

@doc[
  bind.macro '(Array($binding, ...))
]{

 Matches an array with as many elements as @rhombus[binding]s, where
 each element matches its corresponding @rhombus[binding].

@examples[
  val Array(1, x, y): Array(1, 2, 3),
  y
]

}

@doc[
  annotation.macro 'Array,
  annotation.macro '(Array.of($annotation)),
]{

 Matches any array in the form without @rhombus[of]. The @rhombus[of]
 variant matches an array whose elements satisfy @rhombus[annotation].

}
