#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Numbers}

@doc[
  annotation.macro 'Number
]{

  Matches any number.

}

@doc[
  annotation.macro 'Integer
]{

  Matches exact integers.

}

@doc[
  operator ((x :: Number) + (y :: Number)) :: Number,
  operator ((x :: Number) - (y :: Number)) :: Number,
  operator ((x :: Number) * (y :: Number)) :: Number,
  operator ((x :: Number) / (y :: Number)) :: Number
]{

 The usual arithmetic operators with the usual precedence, except that
 @rhombus[/] does not have the same precedence as @rhombus[*] when it
 appears to the right of @rhombus[*].

@examples[
  1+2,
  3-4,
  5*6,
  8/2,
  1+2*3
]

}

@doc[
  operator ((x :: Number) > (y :: Number)) :: Boolean,
  operator ((x :: Number) >= (y :: Number)) :: Boolean,
  operator ((x :: Number) < (y :: Number)) :: Boolean,
  operator ((x :: Number) <= (y :: Number)) :: Boolean
]{

 The usual comparsion operators.

@examples[
  1 < 2,
  3 >= 3.0
]

}
