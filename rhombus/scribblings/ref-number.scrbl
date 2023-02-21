#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Numbers}

@doc(
  annot.macro 'Number'
){

  Matches any number.

}

@doc(
  annot.macro 'Int'
  annot.macro 'PosInt'
  annot.macro 'NegInt'
  annot.macro 'NonnegInt'
){

 Matches exact integers: all of them, positive integers, negative
 integers, or nonnegative integers.

}

@doc(
  annot.macro 'Real'
){

 Matches real numbers (as opposed to imaginary numbers like the result
 of @rhombus(math.sqrt(-1))).

}

@doc(
  annot.macro 'Flonum'
){

 Matches real numbers that are represented as floating-point numbers.

}

@doc(
  annot.macro 'Byte'
){

 Matches integers in the range @rhombus(0) ro @rhombus(255) (inclusive).

}

@doc(
  operator ((x :: Number) + (y :: Number)) :: Number
  operator ((x :: Number) - (y :: Number)) :: Number
  operator ((x :: Number) * (y :: Number)) :: Number
  operator ((x :: Number) / (y :: Number)) :: Number
){

 The usual arithmetic operators with the usual precedence, except that
 @rhombus(/) does not have the same precedence as @rhombus(*) when it
 appears to the right of @rhombus(*).

@examples(
  1+2
  3-4
  5*6
  8/2
  1+2*3
)

}

@doc(
  operator ((x :: Number) > (y :: Number)) :: Boolean
  operator ((x :: Number) >= (y :: Number)) :: Boolean
  operator ((x :: Number) < (y :: Number)) :: Boolean
  operator ((x :: Number) <= (y :: Number)) :: Boolean
){

 The usual comparsion operators on numbers. See also @rhombus(.=).

@examples(
  1 < 2
  3 >= 3.0
)

}

@doc(
  fun math.abs(x :: Real) :: Real
  fun math.max(x :: Real) :: Real
  fun math.min(x :: Real) :: Real
  fun math.floor(x :: Real) :: Real
  fun math.ceiling(x :: Real) :: Real
  fun math.round(x :: Real) :: Real
  fun math.sqrt(x :: Number) :: Number
  fun math.log(x :: Number) :: Number
  fun math.exp(x :: Number) :: Number
  fun math.expt(base :: Number, power :: Number) :: Number
  fun math.cos(x :: Number) :: Number
  fun math.sin(x :: Number) :: Number
  fun math.tan(x :: Number) :: Number
  fun math.acos(x :: Number) :: Number
  fun math.asin(x :: Number) :: Number
  fun math.atan(x :: Number) :: Number
  fun math.atan(y :: Number, x :: Number) :: Number
){

 The usual functions on numbers.

@examples(
  math.abs(-1.5)
  math.min(1, 2)
  math.max(1, 2)
  math.floor(1.5)
  math.ceiling(1.5)
  math.round(1.5)
  math.sqrt(4)
  math.cos(3.14)
  math.sin(3.14)
  math.tan(3.14 / 4)
  math.acos(1)
  math.asin(1)
  math.atan(0.5)
  math.atan(1, 2)
  math.log(2.718)
  math.exp(1)
)

}

@doc(
  fun math.random() :: Real
  fun math.random(n :: PosInt) :: NonnegInt
){

 Returns a random number between @rhombus(0.0) (inclusive) and
 @rhombus(1.0) (exclusive) when called with 0 arguments, and returns an
 integer between 0 (inclusive) and @rhombus(n) (exclusive) when called
 with @rhombus(n).

@examples(
  ~fake:
    math.random()
    0.5348255534758128
  ~fake:
    math.random(17)
    13
)

}
