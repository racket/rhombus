#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Font}

@doc(
  class draw.Font():
    constructor (
      ~kind: kind :: Font.Kind = #'default,
      ~name: name :: maybe(String) = #false,
      ~size: size :: Real.in(0.0, 1024.0) = 12.0,
      ~in_pixels: in_pixels :: Any.to_boolean = #false,
      ~style: style :: Font.Style = #'normal,
      ~weight: weight :: Font.Weight = #'normal,
      ~has_underline: has_underline :: Any = #false,
      ~smoothing: smoothing :: Font.Smoothing = #'default,
      ~hinting: hinting :: Font.Hinting = #'aligned,
      ~features: features :: Map.of(Font.FeatureString, Nat) = {},
    )
){

 Creates an object representing a font configuration.

 The @rhombus(kind) argument acts as a default and fallback for
 @rhombus(name). The specific, system-specific font used for a
 @rhombus(kind) can be obtained from @rhombus(Font.kind_to_name).
 For valid names on a system system, use @rhombus(Font.get_names).

 The font @rhombus(size) is in ``points,'' unless @rhombus(in_pixels) is
 true. On Mac OS, 1 point is 1 pixel. On Windows and Unix, 1 point is
 @rhombus(96/72) pixels.

 The @rhombus(style) and @rhombus(weight) arguments select italic,
 slanted, and/or bold. If @rhombus(has_underline) is true, then text is
 drawn as underlined as well.

 The @rhombus(smoothing) argument controls whether and how much
 anti-aliasing is applied to text, and @rhombus(hinting) controls pixel
 alignment. See @rhombus(Font.Smoothing, ~annot) and
 @rhombus(Font.Hinting, ~annot) for more information.

 The @rhombus(features) argument provides access to additional font
 features. See @rhombus(Font.FeatureString, ~annot) for more information.

 A font like an existing one can be constructed using @rhombus(with) and
 the field names @rhombus(kind, ~datum), @rhombus(name, ~datum),
 @rhombus(size, ~datum), @rhombus(in_pixels, ~datum),
 @rhombus(style, ~datum), @rhombus(weight, ~datum),
 @rhombus(has_underline, ~datum), @rhombus(smoothing, ~datum),
 @rhombus(hinting, ~datum), and @rhombus(features, ~datum).

}

@doc(
  property (font :: draw.Font).kind :: Font.Kind
  property (font :: draw.Font).name :: maybe(String)
  property (font :: draw.Font).size :: Real.in(0.0, 1024.0)
  property (font :: draw.Font).in_pixels :: Boolean
  property (font :: draw.Font).style :: Font.Style
  property (font :: draw.Font).weight :: Font.Weight
  property (font :: draw.Font).has_underline :: Boolean
  property (font :: draw.Font).smoothing :: Font.Smoothing
  property (font :: draw.Font).hinting :: Font.Hinting
  property (font :: draw.Font).features
    :: Map.of(Font.FeatureString, Nat)
){

 Properties to access font components.

}

@doc(
  annot.macro 'draw.Font.FeatureString'
){

 Satisfied by a 4-character string containing only @litchar{ } (i.e., a
 space), @litchar{!}, or characters with @rhombus(Char.to_int) values
 between that of @litchar{#} and @litchar{~}, inclusive.

 The @rhombus(Font.features) property of a font object is a map of
 @hyperlink("https://practicaltypography.com/opentype-features.html"){OpenType
  feature} settings to enable or disable optional typographic features of
 OpenType fonts. Each entry in the hash maps a
 @rhombus(Font.FeatureString, ~annot) feature tag to its desired value.
 For boolean OpenType features, a value of 0 means ``disabled'' and a
 value of 1 means ``enabled''; for other features, the meaning of the
 value varies (and may even depend on the font itself).

}


@doc(
  enum draw.Font.Kind
  | default
  | decorative
  | roman
  | script
  | swiss
  | modern
  | symbol
  | system
){

 A font kind, which is a fallback or default for a font name. The
 specific, system-specific font used for a @rhombus(Font.Kind, ~annot)
 can be obtained from @rhombus(Font.kind_to_name).

}

@doc(
  enum draw.Font.Style
  | normal
  | slant
  | italic
){

 A font style. Typically, @rhombus(#'slant) is a skewed form of a face's
 base shape, while @rhombus(#'italic) can be a different shape.

}

@doc(
  enum draw.Font.Weight
  | ~is_a Int.in(100 ..= 1000)
  | thin
  | ultralight
  | light
  | semilight
  | book
  | normal
  | medium
  | semibold
  | bold
  | ultrabold
  | heavy
  | ultraheavy
){

 A font weight, either an integer in @rhombus(100) to @rhombus(1000)
 (inclusive) or one of the following symbols:

@itemlist(
  @item{@rhombus(#'thin) (equivalent to @rhombus(100))}
  @item{@rhombus(#'ultralight) (equivalent to @rhombus(200))}
  @item{@rhombus(#'light) (equivalent to @rhombus(300))}
  @item{@rhombus(#'semilight) (equivalent to @rhombus(350))}
  @item{@rhombus(#'book) (equivalent to @rhombus(380))}
  @item{@rhombus(#'normal) (equivalent to @rhombus(400))}
  @item{@rhombus(#'medium) (equivalent to @rhombus(500))}
  @item{@rhombus(#'semibold) (equivalent to @rhombus(600))}
  @item{@rhombus(#'bold) (equivalent to @rhombus(700))}
  @item{@rhombus(#'ultrabold) (equivalent to @rhombus(800))}
  @item{@rhombus(#'heavy) (equivalent to @rhombus(900))}
  @item{@rhombus(#'ultraheavy) (equivalent to @rhombus(1000))}
)

}

@doc(
  enum draw.Font.Smoothing
  | default
  | partly_smoothed
  | smoothed
  | unsmoothed
){

 A font smoothing (anti-aliasing) mode:

@itemlist(

 @item{@rhombus(#'default): Platform-specific, sometimes
  user-configurable.}

 @item{@rhombus(#'partly_smoothed): Grayscale anti-aliasing.}

 @item{@rhombus(#'smoothed): Sub-pixel anti-aliasing.}

 @item{@rhombus(#'unsmoothed): No anti-aliasing.}

)

}

@doc(
  enum draw.Font.Hinting
  | aligned
  | unaligned
){

 Whether font metrics should be rounded to integers:

@itemlist(

 @item{@rhombus(#'aligned) (the default): Rounds to integers to improve
  the consistency of letter spacing for pixel-based targets, but at the
  expense of making metrics less precisely scalable.}

 @item{@rhombus(#'unaligned): Disables rounding.}

)

}

@doc(
  fun draw.Font.get_names(
    ~kind: kind :: Font.NameListKind = #'all,
    ~detail: detail :: Font.NameListDetail = #'face
  ) :: List.of(String)

  enum draw.Font.NameListKind
  | all
  | mono

  enum draw.Font.NameListDetail
  | face
  | font
){

 Returns a list of font names available on the current system. If
 @rhombus(kind) is @rhombus(#'mono), then only names that are known to
 correspond to monospace fonts are included in the list.

 If @rhombus(detail) is @rhombus(#'face), then the result is in more
 standard terminology a list of typefaces, which are combined with style
 and weight options to arrive at a font. If @rhombus(detail) is
 @rhombus(#'font), then the result includes a string for each available
 font.

}

@doc(
  fun draw.Font.kind_to_name(
    kind :: Font.Kind
  ) :: String
){

 Returns the font name on he current system that is used whenever the
 given @rhombus(kind) is specified.

}

@doc(
  property (path :: draw.Font).handle :: Any
  fun draw.Font.from_handle(hand :: Any) :: Font
){

 The @rhombus(Font.handle) property returns a Racket object that
 corresponds to the font for use directly with
 @racketmodname(racket/draw). The @rhombus(Font.from_handle) function
 creates a @rhombus(Font, ~class) from such a Racket object.

}
