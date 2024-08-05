#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{String Formatting}

@doc(
  fun str([elem, ...]) :: String
){

 Equivalent to @rhombus(String.append(to_string(elem), ...)), and
 intended to be used with @litchar("@") notation (see
 @shrubref("at-notation")).

@examples(
  @str{1 + 2 = @(1 + 2)}
)

 In the @litchar{{}} argument for @rhombus(str), use @litchar("@")
 escapes with functions like @rhombus(str.f) or @rhombus(repr) to format
 non-strings values in ways other than the @rhombus(to_string) default.

@examples(
  ~repl:
    @str{The key @repr(#'pi) is mapped to @str.f(math.pi, ~precision: 2).}
  ~defn:
    fun pct(n): str.f(n * 100, ~precision: 1) ++ "%"
  ~repl:
    print(@str{Contents: @pct(0.251) apples
                         @pct(1/3) bananas
                         @pct(0.5) cherries})
)

}

@doc(
  fun str.s(v :: Any,
            ~width: width :: maybe(NonnegInt) = #false,
            ~min_width: min_width :: maybe(NonnegInt) = width,
            ~max_width: max_width :: maybe(NonnegInt) = width,
            ~pad: pad :: Char = " "[0],
            ~align: align :: str.Align = #'left,
            ~clip_align: clip_align :: str.Align = align)
    :: String
){

 Converts @rhombus(v) to a string in the same way as
 @rhombus(to_string), but may adjusts the size of the resulting string if
 @rhombus(min_width) or @rhombus(max_width) is not @rhombus(#false). The
 @rhombus(width) argument serves only as way to provide
 @rhombus(min_width) and @rhombus(max_width) at once. If
 @rhombus(min_width) and @rhombus(max_width) are both provided, an
 exception is thrown if @rhombus(max_width) is less than
 @rhombus(min_width).

@itemlist(

 @item{If the string must be padded to make it at least @rhombus(min_width)
  characters, then copies of the @rhombus(pad) character are added.
  Padding is added to the end if @rhombus(align) is @rhombus(#'left), to
  the start if @rhombus(align) is @rhombus(#'right), and to both ends if
  @rhombus(align) is @rhombus(#'center).}

 @item{If the string must be truncated to reduce it to @rhombus(max_width)
  characters, the initial characters are preserved if @rhombus(clip_align) is
  @rhombus(#'left), trailing characters are preserved if @rhombus(clip_align)
  is @rhombus(#'right), and central character are preserved if
  @rhombus(clip_align) is @rhombus(#'center).}

)

@examples(
  str.s("hello", ~width: 10)
  str.s("hello", ~width: 10, ~pad: "_"[0], ~align: #'center)
  str.s("hello", ~max_width: 2)
  str.s("hello", ~max_width: 2, ~clip_align: #'right)
)


}

@doc(
  fun str.d(n :: Int,
            ~width: width :: maybe(NonnegInt) = #false,
            ~min_width: min_width :: maybe(NonnegInt) = width,
            ~max_width: max_width :: maybe(NonnegInt) = width,
            ~pad: pad :: Char = " "[0],
            ~align: align :: str.Align = #'left,
            ~clip_align: clip_align :: str.Align = align,
            ~minus_sign: minus_sign :: String  = "-",
            ~plus_sign: plus_sign :: String  = "",
            ~zero_sign: zero_sign :: String  = "",
            ~sign_align: sign_align :: str.Align = #'center)
    :: String
  fun str.b(n :: Int, ....) :: String
  fun str.o(n :: Int, ....) :: String
  fun str.x(n :: Int, ....) :: String
  fun str.X(n :: Int, ....) :: String
){

 These functions all accept the same arguments, and they convert an
 integer to string using a specific base: decimal for @rhombus(str.d),
 binary for @rhombus(str.b), octal for @rhombus(str.o), lowercase
 hexadecimal for @rhombus(str.x), and uppercase hexadecimal for
 @rhombus(str.X).

 The @rhombus(min_width), @rhombus(max_width), @rhombus(pad),
 @rhombus(align), and @rhombus(clip_align) arguments are used the same as
 by @rhombus(str.s).

 The @rhombus(minus_sign), @rhombus(plus_sign), and @rhombus(zero_sign)
 arguments determines how a sign is shown. By default, a sign is shown
 only for negative @rhombus(n).

 The @rhombus(sign_align) argument determines where the sign string is
 positioned relative to digits. A @rhombus(#'left) or @rhombus(#'center)
 value places the sign on the left, and @rhombus(#'right) on the right. A
 @rhombus(#'left) or @rhombus(#'right) value further moves the sign to
 before or after any padding that is added to the number to make the
 result @rhombus(min_width) characters, and it preserves the sign and
 clips digits, instead, when clipping characters to fit into
 @rhombus(max_width).

@examples(
  str.d(-10)
  str.x(-10)
  str.d(-10, ~width: 8)
  str.d(-10, ~width: 8, ~align: #'right)
  str.d(-10, ~width: 8, ~align: #'right, ~sign_align: #'left)
  str.d(-123456, ~width: 4, ~align: #'right, ~sign_align: #'left)
  str.d(10, ~plus_sign: "+")
)

}


@doc(
  fun str.f(n :: Real,
            ~precision: precision :: NonnegInt = 6,
            ~keep_decimal: keep_decimal :: Any = #false,
            ~width: width :: maybe(NonnegInt) = #false,
            ~min_width: min_width :: maybe(NonnegInt) = width,
            ~max_width: max_width :: maybe(NonnegInt) = width,
            ~pad: pad :: Char = " "[0],
            ~align: align :: str.Align = #'left,
            ~clip_align: clip_align :: str.Align = align,
            ~minus_sign: minus_sign :: String  = "-",
            ~plus_sign: plus_sign :: String  = "",
            ~zero_sign: zero_sign :: String  = "",
            ~sign_align: sign_align :: str.Align = #'center,
            ~decimal: decimal :: String = ".")
    :: String
  fun str.e(n :: Int, ....,
            ~exponent: exponent :: String = "e") :: String
  fun str.g(n :: Int, ....,
            ~exponent: exponent :: String = "e") :: String
){

 Like @rhombus(str.d), but for a real number that may include a decimal
 point and may be written in exponential notation. The handling of
 optional arguments is the same as in @rhombus(str.d), but
 @rhombus(~precision), @rhombus(~keep_decimal), and @rhombus(~decimal)
 arguments are accepted in addition, and @rhombus(str.e) and
 @rhombus(str.f) also accept an @rhombus(~exponent) argument. The
 functions @rhombus(str.f), @rhombus(str.e), and @rhombus(str.g) produce
 strings in different forms, except that they all represent
 @rhombus(#inf), @rhombus(#neginf), and @rhombus(#nan), as
 @rhombus("inf"), @rhombus("-inf"), and @rhombus("nan"), respectively;
 the rules below apply for other values of @rhombus(n).

 The @rhombus(str.f) (``f'' for ``floating point'') function produces a
 string that uses a decimal point and @rhombus(precision) subsequent
 digits. If @rhombus(precision) is @rhombus(0), then the decimal point is
 dropped unless @rhombus(keep_decimal) is true. The @rhombus(decimal)
 string is used to represent a decimal point.

 The @rhombus(str.e) (``e'' for ``exponent'') function produces a string
 that uses exponential notation indicated with @rhombus(exponent). The
 coefficient is written with a single digit before a decimal point, and
 the digit is always between @litchar{1} and @litchar{9} unless
 @rhombus(n) is zero; @rhombus(precision) digits are shown after the
 decimal point. An exponent is shown after @rhombus(exponent), and a sign
 is included for the exponent only if it is negative.

 The @rhombus(str.g) (``g'' for ``general'') function produces a string
 like @rhombus(str.f) or @rhombus(str.e), depending on a relationship
 between @rhombus(precision) and the exponent @rhombus(expo, ~var) that
 would be written with @rhombus(str.e): if the exponent is at least
 @rhombus(-4) and less than @rhombus(precision), then @rhombus(str.f) is
 used, otherwise @rhombus(str.g) is used. When @rhombus(str.g) uses
 @rhombus(str.f), it adjusts @rhombus(precision) to
 @rhombus(precision - 1 - #,(@rhombus(expo, ~var))). When @rhombus(str.g)
 uses @rhombus(str.e), it adjusts @rhombus(precision) to
 @rhombus(precision-1). Since these adjustments only work in general when
 @rhombus(precision) is positive, a @rhombus(precision) of @rhombus(0)
 passed to @rhombus(str.g) is treated as @rhombus(1).

@examples(
  str.f(123.45)
  str.f(2/3, ~precision: 2)
  str.e(2/3, ~precision: 2)
  str.e(123.45e10, ~precision: 2)
  str.g(2/3, ~precision: 2)
  str.g(123.45e10, ~precision: 2)
  str.g(123.45e10, ~decimal: ",", ~exponent: "E")
)

}

@doc(
  enum str.Align:
    left
    center
    right
){

 Alignment for functions like @rhombus(str.s).

}
