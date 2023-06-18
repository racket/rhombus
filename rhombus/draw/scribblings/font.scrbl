#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Font}

@doc(
  class Font(handle):
    constructor (~kind: kind :: Font.Kind = #'default,
                 ~name: name :: maybe(String) = #false,
                 ~size: size :: Real.in(0.0, 1024.0) = 12.0,
                 ~in_pixels: in_pixels :: Any = #false,
                 ~style: style :: Font.Style = #'normal,
                 ~weight: weight :: Font.Weight = #'normal,
                 ~has_underline: has_underline :: Any = #false,
                 ~smoothing: smoothing :: Font.Smoothing = #'default,
                 ~hinting: hinting :: Font.Hinting = #'aligned,
                 ~features: features :: Map.of(Font.FeatureString, NonnegInt) = {})
){

 Creates a font configuration.

 A font like an existing one can be constructed using @rhombus(with) and
 the field names @rhombus(kind), @rhombus(name), @rhombus(size),
 @rhombus(in_pixels), @rhombus(style), @rhombus(weight),
 @rhombus(has_underline), @rhombus(smoothing), @rhombus(hinting), and
 @rhombus(features).

}

@doc(
  property (font :: Font).kind :: Font.Kind
  property (font :: Font).name :: maybe(String)
  property (font :: Font).size :: Real.in(0.0, 1024.0)
  property (font :: Font).in_pixels :: Boolean
  property (font :: Font).style :: Font.Style
  property (font :: Font).weight :: Font.Weight
  property (font :: Font).has_underline :: Boolean
  property (font :: Font).smoothing :: Font.Smoothing
  property (font :: Font).hinting :: Font.Hinting
  property (font :: Font).features :: Map.of(Font.FeatureString, NonnegInt)
){

 Propeties to access font components.

}

@doc(
  annot.macro 'Font.Kind'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'default)}  
  @item{@rhombus(#'decorative)}
  @item{@rhombus(#'roman)}
  @item{@rhombus(#'script)}
  @item{@rhombus(#'swiss)}
  @item{@rhombus(#'modern)}
  @item{@rhombus(#'symbol)}
  @item{@rhombus(#'system)}
)

}

@doc(
  annot.macro 'Font.Style'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'normal)}  
  @item{@rhombus(#'slant)}
  @item{@rhombus(#'italic)}
)

}

@doc(
  annot.macro 'Font.Weight'
){

 Satisfied by either an integer in @rhombus(100) to @rhombus(1000)
 (inclusive) or one of the following symbols:

@itemlist(
  @item{@rhombus(#'thin) (equivalent to @rhombus(100))}  
  @item{@rhombus(#'ultralight) (equivalent to @rhombus(200))}
  @item{@rhombus(#'light) (equivalent to @rhombus(300)}
  @item{@rhombus(#'semilight) (equivalent to @rhombus(350)}
  @item{@rhombus(#'book) (equivalent to @rhombus(380)}
  @item{@rhombus(#'normal) (equivalent to @rhombus(400)}
  @item{@rhombus(#'medium) (equivalent to @rhombus(500)}
  @item{@rhombus(#'semibold) (equivalent to @rhombus(600)}
  @item{@rhombus(#'bold) (equivalent to @rhombus(700)}
  @item{@rhombus(#'ultrabold) (equivalent to @rhombus(800)}
  @item{@rhombus(#'heavy) (equivalent to @rhombus(900)}
  @item{@rhombus(#'ultraheavy) (equivalent to @rhombus(1000)}
)

}
