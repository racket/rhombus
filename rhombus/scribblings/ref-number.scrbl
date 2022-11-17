#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Numbers}

@doc(
  annot.macro 'Number'
){

  Matches any number.

}

@doc(
  annot.macro 'Integer'
){

  Matches exact integers.

}

@doc(
  annot.macro 'Real'
){

 Matches real numbers (as opposed to imaginary numbers like the result
 of @rhombus(sqrt(-1))).

}

@doc(
  operator ((x :: Number) + (y :: Number)) :: Number,
  operator ((x :: Number) - (y :: Number)) :: Number,
  operator ((x :: Number) * (y :: Number)) :: Number,
  operator ((x :: Number) / (y :: Number)) :: Number
){

 The usual arithmetic operators with the usual precedence, except that
 @rhombus(/) does not have the same precedence as @rhombus(*) when it
 appears to the right of @rhombus(*).

@examples(
  1+2,
  3-4,
  5*6,
  8/2,
  1+2*3
)

}

@doc(
  operator ((x :: Number) > (y :: Number)) :: Boolean,
  operator ((x :: Number) >= (y :: Number)) :: Boolean,
  operator ((x :: Number) < (y :: Number)) :: Boolean,
  operator ((x :: Number) <= (y :: Number)) :: Boolean,
){

 The usual comparsion operators on numbers. See also @rhombus(.=).

@examples(
  1 < 2,
  3 >= 3.0
)

}

@doc(
  fun sqrt(x :: Number) :: Number,
  fun cos(x :: Number) :: Number,
  fun sin(x :: Number) :: Number,
  fun tan(x :: Number) :: Number,
  fun acos(x :: Number) :: Number,
  fun asin(x :: Number) :: Number,
  fun atan(x :: Number) :: Number,
  fun atan(y :: Number, x :: Number) :: Number,
  fun log(x :: Number) :: Number,
  fun exp(x :: Number) :: Number,
  fun expt(base :: Number, power :: Number) :: Number,
  fun floor(x :: Number) :: Number,
  fun ceiling(x :: Number) :: Number,
  fun round(x :: Number) :: Number,
){

 The usual functions on numbers.

@examples(
  sqrt(4),
  cos(3.14),
  sin(3.14),
  tan(3.14 / 4),
  acos(1),
  asin(1),
  atan(0.5),
  atan(1, 2),
  log(2.718),
  exp(1),
  floor(1.5),
  ceiling(1.5),
  round(1.5)
)

}
