#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/random.Random)

@title(~style: #'toc){Numbers}

A number can be an @deftech{integer} (see @rhombus(Integer, ~annot)),
@deftech{rational number} (see @rhombus(Rational, ~annot)), or
@deftech{real number} (see @rhombus(Real, ~annot)), or @deftech{complex
 number} (see @rhombus(Number, ~annot)), where each of those categories
includes the earlier categories. For the last three categories, a number
is either @deftech{exact} (see @rhombus(Exact, ~annot)) or
@deftech{inexact} (see @rhombus(Inexact, ~annot)), where inexact real
numbers are represented using a 64-bit IEEE floating-point
representation. An integer is always exact. Inexact complex numbers have
inexact real and imaginary components, except that the real component
can be exact @rhombus(0). The ``real'' category is something of a
misnomer, since it is rational numbers plus the special constants
@rhombus(#inf), @rhombus(#neginf), and @rhombus(#nan) that correspond to
IEEE floating-point infinities and non-a-number. An inexact real number
is a @tech{flonum}.

The precision and size of exact numbers is limited only by available
memory and the precision of operations that can produce irrational
numbers. Adding, multiplying, subtracting, and dividing exact numbers
always produces an exact result. A @tech{fixnum} (see
@rhombus(Fixnum, ~annot)) is an integer that is small enough to have a
specialized representation.

Real numbers are @tech{comparable}, which means that generic operations
like @rhombus(<) and @rhombus(>) work on real numbers, while specialized
operations like @rhombus(.<) and @rhombus(.>) work only on real numbers.
Two numbers are the same according to @rhombus(==) when they have the
same exactness as well as the same numerical value. Comparison with
@rhombus(.=) conceptually coerces an inexact argument to an exact
representation before comparing with @rhombus(==).

The syntax of numbers is determined by the
@seclink(~doc: shrub_doc, "token-parsing"){shrubbery} layer. Note that
@litchar{_} is allowed as a separator between digits, so
@rhombus(1_000_000) is the same as @rhombus(1000000). Complex number
literals rely on a escape to S-expression syntax.

@doc(
  annot.macro 'Number'
){

 Matches any number.

 Although only @tech{real numbers} are comparable, to make the results of
 arithmetic operations easier to compare in static mode, static
 information associated by @rhombus(Number, ~annot) specifies
 @rhombus(Real, ~annot)-specific @rhombus(Comparable, ~class)
 operations.

@examples(
  5 is_a Number
  5/2 is_a Number
  #inf is_a Number
  math.sqrt(-1) is_a Number
)

}

@doc(
  annot.macro 'Exact'
  annot.macro 'Inexact'
){

 Matches @tech{exact} and @tech{inexact} numbers, respectively. An inexact number is
 one that is represented as a floating-point number or a complex number
 with an inexact real or imaginary part. These two annotations are
 mutually exclusive.

@examples(
  ~check:
    5 is_a Exact
    ~is #true
  ~check:
    5.0 is_a Inexact
    ~is #true
  ~check:
    5.0 is_a Exact
    ~is #false
  ~check:
    5 is_a Inexact
    ~is #false
)

}

@doc(
  ~nonterminal:
    lo_expr: block expr
    hi_expr: block expr
  annot.macro 'Int'
  annot.macro 'PosInt'
  annot.macro 'NegInt'
  annot.macro 'NonnegInt'
  annot.macro 'Nat'

  annot.macro 'Int.in($range)'
  annot.macro 'Int.in($lo_expr $inclusivity, $hi_expr $inclusivity)'

  grammar inclusivity
  | #,(epsilon)
  | ~inclusive
  | ~exclusive
){

 Matches @tech{exact} @tech{integers}: all of them, positive integers, negative
 integers, nonnegative integers, or integers in a given range.
 None that a range can omit a lower bound or upper bound, such as
 when @rhombus(..) is used as a prefix or postfix operator. The
 @rhombus(Nat, ~annot) annotation is the preferred alias for
 @rhombus(NonnegInt, ~annot).

 The @rhombus(Int.in, ~annot) annotation constraints a integers to be
 within the given range, which is specified either by constructing a
 @rhombus(Range, ~annot) or by providing separate start and end numbers.
 When a range is specific by separate numbers, then each end of the range
 is inclusive by default, but @rhombus(~inclusive) or
 @rhombus(~exclusive) can be specified. Using @rhombus(..), @rhombus(..=),
 @rhombus(<..), or @rhombus(<..=) directly to construct a range
 is treated the same as specifying one or two separate numbers, instead of constructing
 an intermediate @rhombus(Range, ~annot) value.

@examples(
  5 is_a Int
  5 is_a PosInt
  -5 is_a NegInt
  0 is_a Nat
  0 is_a Int.in(0 .. 1)
  0 is_a Int.in(0, 1 ~exclusive)
  1 is_a Int.in(0, 1 ~exclusive)
  42 is_a Int.in(10 ..)
  42 is_a Int.in(100 ..)
)

}

@doc(
  ~nonterminal:
    lo_expr: block expr
    hi_expr: block expr
    inclusivity: Int ~annot
  annot.macro 'Real'
  annot.macro 'PosReal'
  annot.macro 'NegReal'
  annot.macro 'NonnegReal'
  annot.macro 'Real.at_least($lo_expr)'
  annot.macro 'Real.above($lo_expr)'
  annot.macro 'Real.below($hi_expr)'
  annot.macro 'Real.at_most($hi_expr)'
  annot.macro 'Real.in($lo_expr $inclusivity, $hi_expr $inclusivity)'
){

 The @rhombus(Real, ~annot) annotation matches @tech{real numbers} (as opposed
 to imaginary numbers like the result of @rhombus(math.sqrt(-1))). The
 @rhombus(PosReal, ~annot), @rhombus(NegReal, ~annot), and
 @rhombus(NonnegReal, ~annot) annotations match positive, negative, and
 non-negative real numbers, respectively.

 The @rhombus(Real.at_least, ~annot), @rhombus(Real.above, ~annot),
 @rhombus(Real.below, ~annot), and @rhombus(Real.at_most, ~annot)
 annotations further constrain the number to be equal to or greater than,
 greater than, less then, or equal to or less than the given number,
 respectively.

 The @rhombus(Real.in, ~annot) annotation constraints a real number to
 be within the given range, where each end of the range is inclusive by
 default, but @rhombus(~inclusive) or @rhombus(~exclusive) can be
 specified.

@examples(
  5 is_a Real
  5.0 is_a Real
  5.1 is_a Real
  #inf is_a Real
  math.sqrt(-1) is_a Real

  1 is_a Real.at_least(1)
  1 is_a Real.above(1)
  0 is_a Real.in(0, 1 ~exclusive)
  1 is_a Real.in(0, 1 ~exclusive)
)

}

@doc(
  annot.macro 'Rational'
){

 Matches @tech{rational numbers}: the same numbers as @rhombus(Real)
 except for @rhombus(#inf), @rhombus(#neginf), and @rhombus(#nan).

@examples(
  5 is_a Rational
  #inf is_a Rational
)

}

@doc(
  annot.macro 'Integral'
){

 Matches the same numbers as @rhombus(Int) plus @tech{real numbers} that have
 no fractional component.

@examples(
  5 is_a Integral
  5.0 is_a Integral
  5.1 is_a Integral
)

}

@doc(
  annot.macro 'Flonum'
){

 Matches @tech{flonums}, which are inexact real numbers that are
 represented as IEEE 64-bit floating-point numbers.

@examples(
  5.0 is_a Flonum
  #inf is_a Flonum
  5 is_a Flonum
  5/2 is_a Flonum
)

}


@doc(
  annot.macro 'Fixnum'
){

 Matches integers that fall within a @tech{fixnum} range.

@examples(
  1024 is_a Fixnum
  2 ** 100 is_a Fixnum
)

}


@doc(
  annot.macro 'Byte'
){

 Matches bytes. A @deftech{byte} is an integer in the range
 @rhombus(0) to @rhombus(255) (inclusive).

@examples(
  5 is_a Byte
  256 is_a Byte
  Byte#"a" is_a Byte
)

}

@doc(
  expr.macro 'Byte $single_byte_bstr'
  repet.macro 'Byte $single_byte_bstr'
  bind.macro 'Byte $single_byte_bstr'
){

 Produces or matches a byte. The @rhombus(single_byte_bstr)
 literal @tech{byte string} must have exactly a single byte, and that
 byte is produced or matched.

@examples(
  Byte#"a"
  match Byte#"a"
  | Byte#"a" || Byte#"b": "yes"
  | ~else: "no"
  ~error:
    Byte#"too long"
  ~error:
    Byte#"a" matches Byte#"too long"
)

}


@doc(
  operator ((x :: Number) + (y :: Number)) :: Number:
    ~order: addition
  operator ((x :: Number) - (y :: Number)) :: Number:
    ~order: addition
  operator (- (x :: Number)) :: Number:
    ~order: addition
  operator ((x :: Number) * (y :: Number)) :: Number:
    ~order: multiplication
  operator ((x :: Number) / (y :: Number)) :: Number:
    ~order: multiplication
  operator ((x :: Number) ** (y :: Number)) :: Number:
    ~order: exponentiation
){

 The usual arithmetic operators with the usual precedence.

 When expressions for both @rhombus(x) and @rhombus(y) have static
 information from @rhombus(Flonum, ~annot) (or just @rhombus(x) in the
 case of prefix the @rhombus(-) operator), then the arithmetic operation
 is specialized to one that expects @rhombus(Flonum, ~annot) arguments.
 If the static information is incorrect (e.g., because a non-checking
 @rhombus(:~) is used), then a run-time error is reported if an argument
 is not a @rhombus(Flonum, ~annot).

 Note that forms like @rhombus(+1), @rhombus(-1), and @rhombus(1/2) are
 immediate numbers, as opposed to uses of the @rhombus(+), @rhombus(-),
 and @rhombus(/) operators.

@examples(
  1+2
  3-4
  - 4
  5*6
  8 / 2
  7 / 2
  7.0/2
  1+2*3
  2**10
  6 / 2 * 3
)

}


@doc(
  operator ((x :: Integral) div (y :: Integral)) :: Integral:
    ~order: integer_division
  operator ((x :: Integral) rem (y :: Integral)) :: Integral:
    ~order: integer_division
  operator ((x :: Integral) mod (y :: Integral)) :: Integral:
    ~order: integer_division
){

 Integer division (truncating), remainder, and modulo operations. These
 operators have stronger precedence than @rhombus(+) and @rhombus(-) but
 no precedence relationship to @rhombus(*), @rhombus(/), or @rhombus(**).

@examples(
  7 div 5
  7 rem 5
  7 mod 5
  7 mod -5
)

}


@doc(
  operator ((x :: Real) .> (y :: Real)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Real) .>= (y :: Real)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Real) .< (y :: Real)) :: Boolean:
    ~order: order_comparison
  operator ((x :: Real) .<= (y :: Real)) :: Boolean:
    ~order: order_comparison
){

 The usual comparison operators on real numbers prefixed with @litchar{.} to
 distinguish them from generic operations like @rhombus(<) on
 @tech{comparable} values. See also @rhombus(.=) and @rhombus(.!=),
 which work on all numbers.

 These comparisons are specialized like @rhombus(+) for arguments with
 @rhombus(Flonum, ~annot) static information.

@examples(
  1 .< 2
  3 .>= 3.0
)

}

@doc(
  fun math.abs(x :: Real) :: Real
  fun math.max(x :: Real, y :: Real, ...) :: Real
  fun math.min(x :: Real, y :: Real, ...) :: Real
  fun math.floor(x :: Real) :: Real
  fun math.ceiling(x :: Real) :: Real
  fun math.round(x :: Real) :: Real
  fun math.truncate(x :: Real) :: Real
  fun math.sqrt(x :: Number) :: Number
  fun math.log(x :: Number, base :: Number = math.exp(1))
    :: Number
  fun math.exp(x :: Number) :: Number
  fun math.expt(base :: Number, power :: Number) :: Number
  fun math.cos(x :: Number) :: Number
  fun math.sin(x :: Number) :: Number
  fun math.tan(x :: Number) :: Number
  fun math.acos(x :: Number) :: Number
  fun math.asin(x :: Number) :: Number
  fun math.atan(x :: Number) :: Number
  fun math.atan(y :: Real, x :: Real) :: Number
){

 The usual functions on numbers.

 When a call to the @rhombus(math.abs), @rhombus(math.max),
 @rhombus(math.min), @rhombus(math.floor), @rhombus(math.ceiling),
 @rhombus(math.round), @rhombus(math.truncate), @rhombus(math.sin),
 @rhombus(math.cos), or @rhombus(math.tan) function has arguments with
 static information from @rhombus(Flonum, ~annot), then the functions are
 specialized to ones that require @rhombus(Flonum, ~annot) arguments,
 similar to operations like @rhombus(+). The other functions are not
 specialized, because they do not always produce @rhombus(Flonum, ~annot)
 results for @rhombus(Flonum, ~annot) arguments.

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
  fun math.exact(x :: Number) :: Exact
  fun math.inexact(x :: Number) :: Inexact
){

 Converts a number to ensure that it is @tech{exact} or @tech{inexact}, respectively.
 Some @tech{real numbers} and @tech{complex numbers}, such as @rhombus(#inf), cannot be made exact.

@examples(
  math.exact(5.0)
  math.inexact(5)
)

}


@doc(
  fun math.real_part(x :: Number) :: Real
  fun math.imag_part(x :: Number) :: Real
  fun math.magnitude(x :: Number) :: Real
  fun math.angle(x :: Number) :: Real
){

 Operations on @tech{complex numbers}.

@examples(
  math.real_part(5)
  math.real_part(math.sqrt(-1))
  math.imag_part(math.sqrt(-1))
  math.magnitude(1 + math.sqrt(-1))
  math.angle(1 + math.sqrt(-1))
)

}


@doc(
  fun math.numerator(q :: Rational) :: Integral
  fun math.denominator(q :: Rational) :: Integral
){

 Coerces @rhombus(q) to an @tech{exact} number, finds the numerator of the
 number expressed in its simplest fractional form, and returns this
 number coerced to the exactness of @rhombus(q).

@examples(
  math.numerator(256/6)
  math.denominator(256/6)
  math.denominator(0.125)
)

}

@doc(
  fun math.gcd(q :: Rational, ...) :: Rational
  fun math.lcm(q :: Rational, ...) :: Rational
){

 Coerces each @rhombus(q) to an @tech{exact} number and finds the greatest
 common divisor or least common multiple.

 For non-integer arguments, the result for @rhombus(math.gcd) is the
 greatest common divisor of the numerators divided by the least common
 multiple of the denominators. The result for @rhombus(math.lcm) for
 non-integer arguments is the absolute value of the product divided by
 the @rhombus(math.gcd) of the arguments.

 If no arguments are provided, the result of @rhombus(math.gcd) is
 @rhombus(0) and the result of @rhombus(math.lcm) is @rhombus(1). If all
 arguments for @rhombus(math.gcd) are zero, the result is zero. If any
 argument for @rhombus(math.lcm) is zero, the result is zero, and the
 result is exact @rhombus(0) if any argument is exact @rhombus(0).

@examples(
  math.gcd(2, 3, 7)
  math.gcd(4, 2, 16)
  math.lcm(2, 3, 7)
  math.lcm(4, 10, 2)
  math.gcd()
  math.lcm()
)

}

@doc(
  fun math.random()
    :: Real.in(0 ~exclusive, 1 ~exclusive)
  fun math.random(n :: PosInt)
    :: Int.in(0, n ~exclusive)
  fun math.random(start :: Int, end :: Int)
    :: Int.in(start, end ~exclusive)
){

 When called with no arguments, returns a random @tech{real number} between
 @rhombus(0) (exclusive) and @rhombus(1) (exclusive).

 When called with @rhombus(n), returns a random @tech{integer} between
 @rhombus(0) (inclusive) and @rhombus(n) (exclusive).

 When called with @rhombus(start) and @rhombus(end), returns a random
 integer between @rhombus(start) (inclusive) and @rhombus(end)
 (exclusive).

 See also @rhombus(Random.random).

@examples(
  ~fake:
    math.random()
    0.5348255534758128
  ~fake:
    math.random(17)
    13
  ~fake:
    math.random(1, 42)
    21
)

}

@doc(
  fun math.sum(n :: Number, ...) :: Number
  reducer.macro 'math.sum'
  fun math.product(n :: Number, ...) :: Number
  reducer.macro 'math.product'
){

 Generalizations of @rhombus(+) and @rhombus(*) to any number of
 arguments and to work as a reducer with @rhombus(for).

@examples(
  math.sum()
  math.sum(1, 2, 3, 4)
  math.product(1, 2, 3, 4)
  for math.sum (i in 0..10):
    i
)

}

@doc(
  fun math.equal(n :: Number, ...) :: Boolean
  fun math.less(n :: Real, ...) :: Boolean
  fun math.less_or_equal(n :: Real, ...) :: Boolean
  fun math.greater(n :: Real, ...) :: Boolean
  fun math.greater_or_equal(n :: Real, ...) :: Boolean
){

 Generalizations of @rhombus(.=), @rhombus(<), @rhombus(<=),
 @rhombus(>), and @rhombus(>=) to any number of arguments. The result is
 always @rhombus(#true) for zero or one arguments.

@examples(
  math.less()
  math.less(1)
  math.less(2, 1)
  math.less(1, 2, 3, 4)
  math.less(1, 2, 3, 0)
)

}

@doc(
  def math.pi :: Flonum
){

 An approximation of @italic{π}, the ratio of a circle's circumference to its diameter.

@examples(
  math.pi
)

}

@doc(
  operator ((n :: Int) bits.and (m :: Int)) :: Int:
    ~order: bitwise_conjunction
  operator ((n :: Int) bits.or (m :: Int)) :: Int:
    ~order: bitwise_disjunction
  operator ((n :: Int) bits.xor (m :: Int)) :: Int:
    ~order: bitwise_disjunction
  operator (bits.not (n :: Int)) :: Int:
    ~order: bitwise_negation
  operator ((n :: Int) bits.(<<) (m :: Nat)) :: Int:
    ~order: bitwise_shift
  operator ((n :: Int) bits.(>>) (m :: Nat)) :: Int:
    ~order: bitwise_shift
  operator ((n :: Int) bits.(?) (m :: Nat)) :: Boolean:
    ~order: bitwise_test
  fun bits.length(n :: Int) :: Int
  fun bits.field(n :: Int,
                 start :: Nat,
                 end :: Nat)
    :: Int
){

 Bitwise operations on @tech{integers} interpreted as a semi-infinite two's
 complement representation:

@itemlist(

  @item{@rhombus(bits.and), @rhombus(bits.or) (inclusive),
   @rhombus(bits.xor), work on pairs of bits from @rhombus(n) and
   @rhombus(m);}

  @item{@rhombus(bits.not) inverts every bit in @rhombus(n);}

  @item{@rhombus(bits.(<<)) and @rhombus(bits.(>>)) perform a left shift
   or arithmetic right shift of @rhombus(n) by @rhombus(m) bits;}

  @item{@rhombus(bits.(?)) reports whether the bit at position
   @rhombus(m) (counting from 0 as the least-significant bit) is set within
   @rhombus(n);}

  @item{@rhombus(bits.length) reports the number of bits that remain in
   @rhombus(n) if all identical leading bits (@rhombus(0)s for a position
   @rhombus(n) or @rhombus(1)s for a negative @rhombus(n)) are removed;
   and}

  @item{@rhombus(bits.field) produces the integer represented by bits
   @rhombus(start) (inclusive) through @rhombus(end) (exclusive) of
   @rhombus(n).}

)

@examples(
  5 bits.and 3
  5 bits.or 3
  5 bits.xor 3
  bits.not 3
  5 bits.(<<) 2
  5 bits.(>>) 2
  bits.length(2)
  bits.length(-2)
  bits.field(255, 1, 4)
)

}

@include_section("flonum.scrbl")
@include_section("fixnum.scrbl")
