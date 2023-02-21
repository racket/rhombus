#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(fun UCat(x): x)

@title{Characters}

A @deftech{character} is Unicode code point.


@doc(
  annot.macro 'Char'
){

  Matches characters.

}

@doc(
  fun Char.to_int(ch :: Char) :: NonnegInt
){

 Returns the Unicode value of a character.

}

@doc(
  fun Char.from_int(i :: NonnegInt) :: Char
){

 Returns the character corresponding to a Unicode value. The given
 @rhombus(i) must be in the range @rhombus(0x0) to @rhombus(0x10FFFF),
 (inclusive), and not in the range @rhombus(0xD800) to @rhombus(0xDFFF)
 (inclusive).

}

@doc(
  fun Char.upcase(ch :: Char) :: Char
  fun Char.downcase(ch :: Char) :: Char
  fun Char.foldcase(ch :: Char) :: Char
  fun Char.titlecase(ch :: Char) :: Char
){

 Unicode case conversions.

}

@doc(
  fun Char.is_alphabetic(ch :: Char) :: Boolean
  fun Char.is_lowercase(ch :: Char) :: Boolean
  fun Char.is_uppercase(ch :: Char) :: Boolean
  fun Char.is_titlecase(ch :: Char) :: Boolean
  fun Char.is_numeric(ch :: Char) :: Boolean
  fun Char.is_symbolic(ch :: Char) :: Boolean
  fun Char.is_punctuation(ch :: Char) :: Boolean
  fun Char.is_graphic(ch :: Char) :: Boolean
  fun Char.is_whitespace(ch :: Char) :: Boolean
  fun Char.is_blank(ch :: Char) :: Boolean
  fun Char.is_extended_pictographic(ch :: Char) :: Boolean
  fun Char.general_category(ch :: Char) :: Symbol
  fun Char.grapheme_break_property(ch :: Char) :: Symbol
){

 Character Unicode classifications:

@itemlist(

 @item{alphabetic: Unicode ``Alphabetic'' property}

 @item{lowercase: Unicode ``Lowercase'' property}

 @item{uppercase: Unicode ``Uppercase'' property}

 @item{titlecase: Unicode general category @UCat{Lt}}

 @item{numeric: Unicode ``Numeric_Type'' property other than
  @litchar{None}}

 @item{symbolic: Unicode general category @UCat{Sm}, @UCat{Sc},
  @UCat{Sk}, or @UCat{So}}

 @item{punctuation: Unicode general category @UCat{Pc}, @UCat{Pd},
  @UCat{Ps}, @UCat{Pe}, @UCat{Pi}, @UCat{Pf}, or @UCat{Po}}

 @item{graphic: alphabetic, numeric, symbolic, punctionation, or Unicode
  general category is @UCat{Ll}, @UCat{Lm}, @UCat{Lo}, @UCat{Lt},
  @UCat{Lu}, @UCat{Nd}, @UCat{Nl}, @UCat{No}, @UCat{Mn}, @UCat{Mc}, or
  @UCat{Me}}

 @item{whitespace: Unicode ``White_Space'' property}

 @item{blank (horizontal whitespace): Unicode general category is
  @UCat{Zs} or the Tab characher}

 @item{ISO control: Unicode value between @rhombus(0x0) and
  @rhombus(0x1F) (inclusive) or between @rhombus(0x7F) and @rhombus(0x9F)
  (inclusive)}

 @item{extended pictographic: Unicode ``Extended_Pictographic''
  property}

 @item{general category: @rhombus(#'lu), @rhombus(#'ll), @rhombus(#'lt),
  @rhombus(#'lm), @rhombus(#'lo), @rhombus(#'mn), @rhombus(#'mc),
  @rhombus(#'me), @rhombus(#'nd), @rhombus(#'nl), @rhombus(#'no),
  @rhombus(#'ps), @rhombus(#'pe), @rhombus(#'pi), @rhombus(#'pf),
  @rhombus(#'pd), @rhombus(#'pc), @rhombus(#'po), @rhombus(#'sc),
  @rhombus(#'sm), @rhombus(#'sk), @rhombus(#'so), @rhombus(#'zs),
  @rhombus(#'zp), @rhombus(#'zl), @rhombus(#'cc), @rhombus(#'cf),
  @rhombus(#'cs), @rhombus(#'co), or @rhombus(#'cn).}

 @item{grapheme break property: @rhombus(#'Other), @rhombus(#'CR),
  @rhombus(#'LF), @rhombus(#'Control), @rhombus(#'Extend),
  @rhombus(#'ZWJ), @rhombus(#'Regional_Indicator), @rhombus(#'Prepend),
  @rhombus(#'SpacingMark), @rhombus(#'L), @rhombus(#'V), @rhombus(#'T),
  @rhombus(#'LV), or @rhombus(#'LVT)}

)

}

@doc(
  fun Char.grapheme_step(ch :: Char,
                         state :: Int) :: values(Boolean, Int)
){

 Encodes a state machine for Unicode's grapheme-cluster specification on
 a sequence of code points. It accepts a character for the next code
 point in a sequence, and it returns two values: whether a (single)
 grapheme cluster has terminated since the most recently reported
 termination (or the start of the stream), and a new state to be used
 with @rhombus(Char.grapheme_step) and the next character.

 A value of @rhombus(0) for @rhombus(state) represents the initial state
 or a state where no characters are pending toward a new boundary. Thus,
 if a sequence of characters is exhausted and accumulated @rhombus(state)
 is not @rhombus(0), then the end of the stream creates one last
 grapheme-cluster boundary. When @rhombus(Char.grapheme_step) produces a
 true value as its first result and a non-@rhombus(0) value as its second
 result, then the given @rhombus(ch) must be the only character pending
 toward the next grapheme cluster (by the rules of Unicode grapheme
 clustering).

 The @rhombus(Char.grapheme_step) procedure will produce a result for any
 fixnum @rhombus(state), but the meaning of a non-@rhombus(0)
 @rhombus(state) is specified only in that providing such a state
 produced by @rhombus(Char.grapheme_step) in another call to
 @rhombus(Char.grapheme_step) continues detecting grapheme-cluster
 boundaries in the sequence.

 See also @rhombus(String.grapheme_span) and
 @rhombus(String.grapheme_count).

}
