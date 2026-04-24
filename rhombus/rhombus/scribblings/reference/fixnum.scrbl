#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/fixnum
      rhombus/unsafe.use_unsafe)

@title{Fixnums}

@docmodule(rhombus/fixnum)

A @deftech{fixnum} is an integer that fits into a range that has a
specialized representation. This range depends on the architecture's
word size and Racket implementation, but it corresponds to integers that
fit into a two's complement representation using either 30, 31, 61, or 63
bits. In some cases, using functions and operators from
@rhombusmodname(rhombus/fixnum) can reduce run time, but performance
improvements (if any) may require unsafe mode via @rhombus(use_unsafe).

The functions exported by @rhombusmodname(rhombus/fixnum) explicitly
require fixnum arguments and produce fixnum results.

Some operations, like @rhombus(<), statically specialize for performance
when all arguments have static information for the
@rhombus(Fixnum, ~annot) annotation. There is no performance difference
between that inferred specialization and these operators, and a
conversion is inferred only when the result value is the same for
unspecialized and fixnum-specific operations.

Two fixnums that are the same according to @rhombus(==) are also the
same according to @rhombus(===).

@(~version_at_least "8.14.0.4")

@doc(
  operator ((x :: Fixnum) fixnum.(+) (y :: Fixnum)) :: Fixnum:
    ~order: addition
  operator ((x :: Fixnum) fixnum.(-) (y :: Fixnum)) :: Fixnum:
    ~order: addition
  operator (fixnum.(-) (x :: Fixnum)) :: Fixnum:
    ~order: addition
  operator ((x :: Fixnum) fixnum.(*) (y :: Fixnum)) :: Fixnum:
    ~order: multiplication
  operator ((x :: Fixnum) fixnum.div (y :: Fixnum)) :: Fixnum:
    ~order: integer_division
  operator ((x :: Fixnum) fixnum.rem (y :: Fixnum)) :: Fixnum:
    ~order: integer_division
  operator ((x :: Fixnum) fixnum.mod (y :: Fixnum)) :: Fixnum:
    ~order: integer_division
){

 The same as operators like @rhombus(+), but restricted to @tech{fixnum}
 arguments and results. When the result could not be a fixnum, an
 exception is thrown in safe mode, and the result is unspecified in
 unsafe mode via @rhombus(use_unsafe).

}

@doc(
  operator ((x :: Fixnum) fixnum.(<) (y :: Fixnum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Fixnum) fixnum.(<=) (y :: Fixnum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Fixnum) fixnum.(==) (y :: Fixnum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Fixnum) fixnum.(!=) (y :: Fixnum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Fixnum) fixnum.(>=) (y :: Fixnum)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Fixnum) fixnum.(>) (y :: Fixnum)) :: Boolean:
    ~order: order_comparison
){

 The same as operators like @rhombus(<), but restricted to @tech{fixnum}
 arguments. Note that operators like @rhombus(<) statically specialize for
 performance when all arguments have static information associated with
 the @rhombus(Fixnum, ~annot) annotation.

}

@doc(
  fun fixnum.abs(x :: Fixnum) :: Fixnum
  fun fixnum.min(x :: Fixnum, y :: Fixnum, ...) :: Fixnum
  fun fixnum.max(x :: Fixnum, y :: Fixnum, ...) :: Fixnum
){

 The same as functions like @rhombus(math.abs), but restricted to
 @tech{fixnum} arguments and results.
}

@doc(
  operator ((x :: Fixnum) fixnum.bits.(<<) (y :: Fixnum)) :: Fixnum:
    ~order: bitwise_shift
  operator ((x :: Fixnum) fixnum.bits.(>>) (y :: Fixnum)) :: Fixnum:
    ~order: bitwise_shift
  operator ((x :: Fixnum) fixnum.bits.logical.(>>) (y :: Fixnum))
    :: Fixnum:
      ~order: bitwise_shift
  operator ((x :: Fixnum) fixnum.bits.and (y :: Fixnum)) :: Fixnum:
    ~order: bitwise_conjunction
  operator ((x :: Fixnum) fixnum.bits.or (y :: Fixnum)) :: Fixnum:
    ~order: bitwise_disjunction
  operator ((x :: Fixnum) fixnum.bits.xor (y :: Fixnum)) :: Fixnum:
    ~order: bitwise_disjunction
  operator ((x :: Fixnum) fixnum.bits.not (y :: Fixnum)) :: Fixnum:
    ~order: bitwise_negation
){

 The same as operators like @rhombus(bits.(<<)), but restricted to
 @tech{fixnum} arguments and results. Like @rhombus(fixnum.(+)), when the
 result would not be a fixnum, an exception is thrown in safe mode.

 The @rhombus(fixnum.bits.logical.(>>)) operator is like
 @rhombus(fixnum.bits.(>>)), but it fills high bits with @rhombus(0)
 instead of the fixnum's sign bit.

}

@doc(
  fun fixnum.bits.popcount(x :: Fixnum) :: Fixnum
  fun fixnum.bits.popcount16(x :: Fixnum && Int.in(0 ..= 65535))
    :: Fixnum
  fun fixnum.bits.popcount32(x :: Fixnum && Int.in(0 ..= 4294967295))
    :: Fixnum
){

 Returns the number of bits that are set in the fixnum representation of
 @rhombus(x). Depending on the host system,
 @rhombus(fixnum.bits.popcount16) and/or @rhombus(fixnum.bits.popcount32)
 may be faster than @rhombus(fixnum.bits.popcount) in cases where the
 argument is known to fit into 16 bits or 32 bits.

}

@doc(
  operator ((x :: Fixnum) fixnum.wraparound.(+) (y :: Fixnum))
    :: Fixnum:
      ~order: addition
  operator ((x :: Fixnum) fixnum.wraparound.(-) (y :: Fixnum))
    :: Fixnum:
      ~order: addition
  operator (fixnum.wraparound.(-) (x :: Fixnum))
    :: Fixnum:
      ~order: addition
  operator ((x :: Fixnum) fixnum.wraparound.(*) (y :: Fixnum))
    :: Fixnum:
      ~order: multiplication
  operator ((x :: Fixnum) fixnum.wraparound.bits.(<<) (y :: Fixnum))
    :: Fixnum:
      ~order: bitwise_shift
){

 Like @rhombus(fixnum.(+)), @rhombus(fixnum.(-)), @rhombus(fixnum.(*)),
 and @rhombus(fixnum.bits.(<<)), but if the result would not fit into a
 fixnum, high bits are dropped to make it fit (so an exception is never
 thrown when fixnum arguments are provided).

}


@doc(
  fun fixnum.from_flonum(x :: Flonum) :: Fixnum
  fun fixnum.to_flonum(x :: Fixnum) :: Flonum
){

 Conversions between @tech{fixnum} and @tech{flonum} values.

}


@doc(
  annot.macro 'fixnum.Array'
){

 Matches any @deftech{fixnum array}, which is like a normal
 @tech{array}, but contains only @tech{fixnums}.
 A fixnum array is always mutable, but it does not satisfy
 @rhombus(Array, ~annot) or @rhombus(MutableArray, ~annot).

 A fixnum array potentially provides better performance than a normal
 array of fixnums, because the elements are known to be fixnums without a
 check on access.

 Like an array, a fixnum array is @tech{indexable} using @brackets,
 assignable via @brackets and @rhombus(:=), supports @tech{membership
  tests} using the @rhombus(in) operator, and can be used as
 @tech{sequence}. Two fixnum arrays are equal by @rhombus(is_now) as long
 as they have the same length and their elements are pairwise equal by
 @rhombus(fixnum.(==)).

}

@doc(
  fun fixnum.Array(x :: Fxnum, ...) :: fixnum.Array
  fun fixnum.Array.make(len :: Nat, x :: Fixnum = 0) :: fixnum.Array
){

 Like @rhombus(Array) and @rhombus(Array.make), but creates a
 @tech{fixnum array}.

}

@doc(
  method (arr :: fixnum.Array).length() :: Int
  method (arr :: fixnum.Array).get(n :: Nat) :: Fixnum
  method (arr :: fixnum.Array).set(n :: Nat, x :: Fixnum) :: Void
  method (arr :: fixnum.Array).contains(v :: Any) :: Boolean
  method (arr :: fixnum.Array).copy(
    start :: Nat = 0,
    end :: Nat = arr.length()
  ) :: fixnum.Array
  method (arr :: fixnum.Array).to_list() :: List.of(Fixnum)
  method (arr :: fixnum.Array).to_sequence()
    :: Sequence.assume_of(Fixnum)
){

 Like @rhombus(Array.length), @rhombus(Array.get), @rhombus(Array.set),
 @rhombus(Array.contains), @rhombus(Array.copy), @rhombus(Array.to_list),
 and @rhombus(Array.to_sequence), but for @tech{fixnum arrays}.

 The @rhombus(fixnum.Array.contains) method accepts any argument value
 to find, but it will only succeed for fixnums. The comparsion operation
 is always @rhombus(fixnum.(==)).

}
