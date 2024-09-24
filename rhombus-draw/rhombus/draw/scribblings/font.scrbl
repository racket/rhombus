#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Font}

@doc(
  class draw.Font():
    constructor (
      ~kind: kind :: draw.Font.Kind = #'default,
      ~name: name :: maybe(String) = #false,
      ~size: size :: Real.in(0.0, 1024.0) = 12.0,
      ~in_pixels: in_pixels :: Any = #false,
      ~style: style :: draw.Font.Style = #'normal,
      ~weight: weight :: draw.Font.Weight = #'normal,
      ~has_underline: has_underline :: Any = #false,
      ~smoothing: smoothing :: draw.Font.Smoothing = #'default,
      ~hinting: hinting :: draw.Font.Hinting = #'aligned,
      ~features:
        features :: Map.of(draw.Font.FeatureString, NonnegInt) = {},
    )
){

 Creates a font configuration.

 A font like an existing one can be constructed using @rhombus(with) and
 the field names @rhombus(kind), @rhombus(name), @rhombus(size),
 @rhombus(in_pixels), @rhombus(style), @rhombus(weight),
 @rhombus(has_underline), @rhombus(smoothing), @rhombus(hinting), and
 @rhombus(features).

}

@doc(
  property (font :: draw.Font).kind :: draw.Font.Kind
  property (font :: draw.Font).name :: maybe(String)
  property (font :: draw.Font).size :: Real.in(0.0, 1024.0)
  property (font :: draw.Font).in_pixels :: Boolean
  property (font :: draw.Font).style :: draw.Font.Style
  property (font :: draw.Font).weight :: draw.Font.Weight
  property (font :: draw.Font).has_underline :: Boolean
  property (font :: draw.Font).smoothing :: draw.Font.Smoothing
  property (font :: draw.Font).hinting :: draw.Font.Hinting
  property (font :: draw.Font).features
    :: Map.of(draw.Font.FeatureString, NonnegInt)
){

 Propeties to access font components.

}

@doc(
  annot.macro 'draw.Font.FeatureString'
){

 Satisfied by a 4-character string containing only @litchar{ } (i.e., a
 space), @litchar{!}, or characters with @rhombus(Char.to_int) values
 between that of @litchar{#} and @litchar{~}, inclusive.

}
  

@doc(
  enum draw.Font.Kind:
    default
    decorative
    roman
    script
    swiss
    modern
    symbol
    system
){

 A font kind.

}

@doc(
  enum draw.Font.Style:
    normal
    slant
    italic
){

 A font style.

}

@doc(
  enum draw.Font.Weight:
    ~is_a Int.in(100, 1000 ~inclusive)
    thin
    ultralight
    light
    semilight
    book
    normal
    medium
    semibold
    bold
    ultrabold
    heavy
    ultraheavy
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
  enum draw.Font.Smoothing:
    default
    partly_smoothed
    smoothed
    unsmoothed
){

 A font smoothing (anti-aliasing) mode.

}

@doc(
  enum draw.Font.Hinting:
    aligned
    unaligned
){

 A font hinting (to adjust anti-aliasing) mode.

}
