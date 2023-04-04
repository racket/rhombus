#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Font}

@doc(
  class Font(handle):
    constructor (~like like :: Maybe(Font) = #false,
                 ~kind: kind :: Font.Kind = #'default,
                 ~name: name :: Maybe(String) = #false,
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

 If @rhombus(like) is provided as a @rhombus(Font), then @rhombus(like)
 provides default values for other arguments, instead of the normal
 defaults.

}

@doc(
  property Font.kind(font :: Font) :: Font.Kind
  property Font.name(font :: Font) :: Maybe(String)
  property Font.size(font :: Font) :: Real.in(0.0, 1024.0)
  property Font.in_pixels(font :: Font) :: Boolean
  property Font.style(font :: Font) :: Font.Style
  property Font.weight(font :: Font) :: Font.Weight
  property Font.has_underline(font :: Font) :: Boolean
  property Font.smoothing(font :: Font) :: Font.Smoothing
  property Font.hinting(font :: Font) :: Font.Hinting
  property Font.features(font :: Font) :: Map.of(Font.FeatureString, NonnegInt)
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
