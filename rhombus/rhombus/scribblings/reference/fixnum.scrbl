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
fit into a two's complete representation using either 30, 31, 61, or 63
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
  fun fixnum.bits.popcount16(x :: Fixnum && Int.in(0, 65535))
    :: Fixnum
  fun fixnum.bits.popcount32(x :: Fixnum && Int.in(0, 4294967295))
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
