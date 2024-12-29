#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/flonum)

@title{Flonums}

@docmodule(rhombus/flonum)

A @deftech{flonum} is a number that is represented using an IEEE 64-bit
floating-point representation. A flonum is a plain number, so number
operations like @rhombus(+) work on flonums and mixtures of flonums with
other numbers. In some cases, however, using functions and operators
from @rhombusmodname(rhombus/flonum)can reduce run time by allowing
flonum operations to more efficiently communicate intermediate results.

The functions exported by @rhombusmodname(rhombus/flonum) explicitly
require flonum arguments and produce flonum results. They can produce
different results from functions like @rhombus(math.sqrt), typically
returning @rhombus(#nan) when the result would not be a flonum, such as
when a negative number is passed to @rhombus(math.sqrt).

Some operations, like @rhombus(+), statically specialize for performance
when all arguments have static information for the
@rhombus(Flonum, ~annot) annotation. There is no performance difference
between that inferred specialization and these operators, and a
conversion is inferred only when the result value is the same for
unspecialized and flonum-specific operations.

@(~version_at_least "8.14.0.4")

@doc(
  operator ((x :: Flonum) flonum.(+) (y :: Flonum)) :: Flonum
  operator ((x :: Flonum) flonum.(-) (y :: Flonum)) :: Flonum
  operator (flonum.(-) (x :: Flonum)) :: Flonum
  operator ((x :: Flonum) flonum.(*) (y :: Flonum)) :: Flonum
  operator ((x :: Flonum) flonum.(/) (y :: Flonum)) :: Flonum
  operator ((x :: Flonum) flonum.(**) (y :: Flonum)) :: Flonum
){

 The same as operators like @rhombus(+), but restricted to @tech{flonum}
 arguments and results. Note that operators like @rhombus(+) statically
 specialize for performance when all arguments have static information
 associated with the @rhombus(Flonum, ~annot) annotation.

}

@doc(
  operator ((x :: Flonum) flonum.(<) (y :: Flonum)) :: Boolean
  operator ((x :: Flonum) flonum.(<=) (y :: Flonum)) :: Boolean
  operator ((x :: Flonum) flonum.(==) (y :: Flonum)) :: Boolean
  operator ((x :: Flonum) flonum.(!=) (y :: Flonum)) :: Boolean
  operator ((x :: Flonum) flonum.(>=) (y :: Flonum)) :: Boolean
  operator ((x :: Flonum) flonum.(>) (y :: Flonum)) :: Boolean
){

 The same as operators like @rhombus(<), but restricted to @tech{flonum}
 arguments. Note that operators like @rhombus(<) statically specialize for
 performance when all arguments have static information associated with
 the @rhombus(Flonum, ~annot) annotation.

}

@doc(
  fun flonum.abs(x :: Flonum) :: Flonum
  fun flonum.min(x :: Flonum, y :: Flonum, ...) :: Flonum
  fun flonum.max(x :: Flonum, y :: Flonum, ...) :: Flonum
  fun flonum.floor(x :: Flonum) :: Flonum
  fun flonum.ceiling(x :: Flonum) :: Flonum
  fun flonum.round(x :: Flonum) :: Flonum
  fun flonum.truncate(x :: Flonum) :: Flonum
  fun flonum.sin(x :: Flonum) :: Flonum
  fun flonum.cos(x :: Flonum) :: Flonum
  fun flonum.tan(x :: Flonum) :: Flonum
  fun flonum.asin(x :: Flonum) :: Flonum
  fun flonum.acos(x :: Flonum) :: Flonum
  fun flonum.atan(x :: Flonum) :: Flonum
  fun flonum.sqrt(x :: Flonum) :: Flonum
  fun flonum.log(x :: Flonum) :: Flonum
  fun flonum.exp(x :: Flonum) :: Flonum
){

 The same as functions like @rhombus(math.abs), but restricted to
 @tech{flonum} arguments and results.

 Some related operations, such as @rhombus(math.sqrt), do not always
 produce flonum results for flonum arguments. In those cases, the
 corresponding function like @rhombus(flonum.sqrt) produces
 @rhombus(#nan), instead. For example, @rhombus(flonum.sqrt(-1.0)) is
 @rhombus(#nan) instead of the complex number @rhombus(#{0.0+1.0i}).

}

@doc(
  fun flonum.to_single(x :: Flonum) :: Flonum
){

 Potentially reduces the precision of a flonum so that it matches a
 number that has a IEEE 32-bit floating-point representation.

}

@doc(
  fun flonum.from_int(n :: Int) :: Flonum
  fun flonum.to_int(x :: Flonum) :: Int
){

 Converts flonums from and to integers. The @rhombus(flonum.from_int)
 function produces @rhombus(#inf) or @rhombus(#neginf) when @rhombus(n)
 has a large enough magnitude. The @rhombus(flonum.to_int) function
 truncates flonums that have a fractional component, and it reports an
 error when given @rhombus(#inf), @rhombus(#neginf), or @rhombus(#nan).

}


@doc(
  fun flonum.bit_field(x :: Flonum,
                       start :: Int.in(0, 64),
                       end :: Int.in(0, 64))
    :: Int
){

 Returns an integer whose bitwise representation matches the bits of
 @rhombus(x)'s IEEE representation from @rhombus(start) (inclusive) to
 @rhombus(end) (exclusive). The @rhombus(end) index must be at least as
 large as @rhombus(start). Bit @rhombus(0) corresponds to the low bit of
 the mantissa, while bit @rhombus(63) is the sign bit.

}
