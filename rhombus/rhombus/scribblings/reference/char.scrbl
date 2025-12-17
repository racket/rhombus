#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(fun UCat(x): x)

@title{Characters}

A @deftech{character} is Unicode code point.

Characters are @tech{comparable}, which means that generic operations
like @rhombus(<) and @rhombus(>) work on characters.

@doc(
  annot.macro 'Char'
){

  Matches characters.

}

@doc(
  expr.macro 'Char $single_char_str'
  repet.macro 'Char $single_char_str'
  bind.macro 'Char $single_char_str'
){

 Produces or matches a character. The @rhombus(single_char_str)
 literal @tech{string} must have exactly a single character, and that
 character is produced or matched.

@examples(
  Char"a"
  match Char"a"
  | Char"a" || Char"b": "yes"
  | ~else: "no"
  ~error:
    Char"too long"
  ~error:
    Char"a" matches Char"too long"
)

}

@doc(
  method (ch :: Char).to_int() :: Nat
){

 Returns the Unicode value of a character.

}

@doc(
  fun Char.from_int(i :: Int.in(0x0 ..= 0x10FFFF)
                      && !Int.in(0xD800 ..= 0xDFFF))
    :: Char
){

 Returns the character corresponding to a Unicode value. The given
 @rhombus(i) must be in the range @rhombus(0x0) to @rhombus(0x10FFFF),
 (inclusive), and not in the range @rhombus(0xD800) to @rhombus(0xDFFF)
 (inclusive).

}

@doc(
  method (ch :: Char).upcase() :: Char
  method (ch :: Char).downcase() :: Char
  method (ch :: Char).foldcase() :: Char
  method (ch :: Char).titlecase() :: Char
){

 Unicode case conversions.

}

@doc(
  method (ch :: Char).is_alphabetic() :: Boolean
  method (ch :: Char).is_lowercase() :: Boolean
  method (ch :: Char).is_uppercase() :: Boolean
  method (ch :: Char).is_titlecase() :: Boolean
  method (ch :: Char).is_numeric() :: Boolean
  method (ch :: Char).is_symbolic() :: Boolean
  method (ch :: Char).is_punctuation() :: Boolean
  method (ch :: Char).is_graphic() :: Boolean
  method (ch :: Char).is_whitespace() :: Boolean
  method (ch :: Char).is_blank() :: Boolean
  method (ch :: Char).is_iso_control() :: Boolean
  method (ch :: Char).is_extended_pictographic() :: Boolean
  method (ch :: Char).general_category() :: Symbol
  method (ch :: Char).grapheme_break_property() :: Symbol
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

 @item{graphic: alphabetic, numeric, symbolic, punctuation, or Unicode
  general category is @UCat{Ll}, @UCat{Lm}, @UCat{Lo}, @UCat{Lt},
  @UCat{Lu}, @UCat{Nd}, @UCat{Nl}, @UCat{No}, @UCat{Mn}, @UCat{Mc}, or
  @UCat{Me}}

 @item{whitespace: Unicode ``White_Space'' property}

 @item{blank (horizontal whitespace): Unicode general category is
  @UCat{Zs} or the Tab character}

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
  method (ch :: Char).grapheme_step(state :: Int)
    :: (Boolean, Int)
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

 The @rhombus(Char.grapheme_step) function will produce a result for any
 fixnum @rhombus(state), but the meaning of a non-@rhombus(0)
 @rhombus(state) is specified only in that providing such a state
 produced by @rhombus(Char.grapheme_step) in another call to
 @rhombus(Char.grapheme_step) continues detecting grapheme-cluster
 boundaries in the sequence.

 See also @rhombus(String.grapheme_span) and
 @rhombus(String.grapheme_count).

}



@doc(
  annot.macro 'CharCI'
){

 A @tech{veneer} for a character that redirects @tech{comparable}
 operations like @rhombus(<) and @rhombus(>) to case-insensitive
 comparisons, equivalent to using @rhombus(Char.foldcase) on each
 character before comparing.

 As always for a veneer, @rhombus(CharCI, ~annot) works only in static
 mode (see @rhombus(use_static)) to help ensure that it has the intended
 effect.

@examples(
  ~hidden:
    use_static
  ~repl:
    Char"a" < Char"B"
    (Char"a" :: CharCI) < (Char"B" :: CharCI)
)

}
