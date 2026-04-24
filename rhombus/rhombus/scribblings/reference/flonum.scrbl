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
other numbers. In some cases, using functions and operators from
@rhombusmodname(rhombus/flonum) can reduce run time by allowing flonum
operations to more efficiently communicate intermediate results.

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
  operator ((x :: Flonum) flonum.(+) (y :: Flonum)) :: Flonum:
    ~order: addition
  operator ((x :: Flonum) flonum.(-) (y :: Flonum)) :: Flonum:
    ~order: addition
  operator (flonum.(-) (x :: Flonum)) :: Flonum:
    ~order: addition
  operator ((x :: Flonum) flonum.(*) (y :: Flonum)) :: Flonum:
    ~order: multiplication
  operator ((x :: Flonum) flonum.(/) (y :: Flonum)) :: Flonum:
    ~order: multiplication
  operator ((x :: Flonum) flonum.(**) (y :: Flonum)) :: Flonum:
    ~order: exponentiation
){

 The same as operators like @rhombus(+), but restricted to @tech{flonum}
 arguments and results. Note that operators like @rhombus(+) statically
 specialize for performance when all arguments have static information
 associated with the @rhombus(Flonum, ~annot) annotation.

}

@doc(
  operator ((x :: Flonum) flonum.(<) (y :: Flonum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Flonum) flonum.(<=) (y :: Flonum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Flonum) flonum.(==) (y :: Flonum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Flonum) flonum.(!=) (y :: Flonum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Flonum) flonum.(>=) (y :: Flonum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Flonum) flonum.(>) (y :: Flonum)) :: Boolean:
    ~order: order_comparison
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
                       start :: Int.in(0 ..= 64),
                       end :: Int.in(0 ..= 64))
    :: Int
){

 Returns an integer whose bitwise representation matches the bits of
 @rhombus(x)'s IEEE representation from @rhombus(start) (inclusive) to
 @rhombus(end) (exclusive). The @rhombus(end) index must be at least as
 large as @rhombus(start). Bit @rhombus(0) corresponds to the low bit of
 the mantissa, while bit @rhombus(63) is the sign bit.

}


@doc(
  annot.macro 'flonum.Array'
){

 Matches any @deftech{flonum array}, which is like a normal
 @tech{array}, but contains only @tech{flonums}.
 A flonum array is always mutable, but it does not satisfy
 @rhombus(Array, ~annot) or @rhombus(MutableArray, ~annot).

 A flonum array potentially provides better performance than a normal
 array of flonums, because the elements are known to be flonums without a
 check on access. In particular, flonum calculations that read and write
 flonum arrays may perform better by reducing allocation compared to
 normal arrays.

 Like an array, a flonum array is @tech{indexable} using @brackets,
 assignable via @brackets and @rhombus(:=), supports @tech{membership
  tests} using the @rhombus(in) operator, and can be used as
 @tech{sequence}. Two flonum arrays are equal by @rhombus(is_now) as long
 as they have the same length and their elements are pairwise equal by
 @rhombus(flonum.(==)).

}

@doc(
  fun flonum.Array(x :: Fxnum, ...) :: flonum.Array
  fun flonum.Array.make(len :: Nat, x :: Flonum = 0.0) :: flonum.Array
){

 Like @rhombus(Array) and @rhombus(Array.make), but creates a
 @tech{flonum array}.

}

@doc(
  method (arr :: flonum.Array).length() :: Int
  method (arr :: flonum.Array).get(n :: Nat) :: Flonum
  method (arr :: flonum.Array).set(n :: Nat, x :: Flonum) :: Void
  method (arr :: flonum.Array).contains(v :: Any) :: Boolean
  method (arr :: flonum.Array).copy(
    start :: Nat = 0,
    end :: Nat = arr.length()
  ) :: flonum.Array
  method (arr :: flonum.Array).to_list() :: List.of(Flonum)
  method (arr :: flonum.Array).to_sequence()
    :: Sequence.assume_of(Flonum)
){

 Like @rhombus(Array.length), @rhombus(Array.get), @rhombus(Array.set),
 @rhombus(Array.contains), @rhombus(Array.copy), @rhombus(Array.to_list),
 and @rhombus(Array.to_sequence), but for @tech{flonum arrays}.

 The @rhombus(flonum.Array.contains) method accepts any argument value
 to find, but it will only succeed for flonums. The comparsion operation
 is always @rhombus(flonum.(==)).

}
