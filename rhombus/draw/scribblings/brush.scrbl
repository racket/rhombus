#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Brush}

@doc(
  class Brush(handle, private saved_stipple):
    constructor (~like: like :: Maybe(Brush) = #false,
                 ~color: color :: (String || Color) = "Black",
                 ~style: style :: Brush.Style = #'solid,
                 ~stipple: stipple :: Maybe(Bitmap) = #false,
                 ~gradient: gradient :: Maybe(LinearGradient || RadialGradient)
                              = #false)
){

 Creates a brush configuration.

 If @rhombus(like) is provided as a @rhombus(Brush, ~class), then @rhombus(like)
 provides default values for other arguments, instead of the normal defaults.

}

@doc(
  property (brush :: Brush).color :: Color
  property (brush :: Brush).style :: Brush.Style
  property (brush :: Brush).stipple :: Maybe(Bitmap)
  property (brush :: Brush).gradient :: Maybe(LinearGradient
                                                || RadialGradient)
){

 Properties to access brush components.

}

@doc(
  annot.macro 'Brush.Style'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'transparent)}  
  @item{@rhombus(#'solid)}
  @item{@rhombus(#'opaque)}
  @item{@rhombus(#'xor)}
  @item{@rhombus(#'hilite)}
  @item{@rhombus(#'panel)}
  @item{@rhombus(#'bdiagonal_hatch)}
  @item{@rhombus(#'crossdiag_hatch)}
  @item{@rhombus(#'fdiagonal_hatch)}
  @item{@rhombus(#'cross_hatch)}
  @item{@rhombus(#'horizontal_hatch)}
  @item{@rhombus(#'vertical_hatch)}
)

}


@doc(
  def Brush.none :: Brush
){

 A brush with style @rhombus(#'transparent).

}

@doc(
  class LinearGradient():
    constructor (x0 :: Real, y0 :: Real,
                 x1 :: Real, y1 :: Real,
                 [[stop :: Real.in(0.0, 1.0), color :: Color], ...])
  property (grad :: LinearGradient).line
    :: matching([_ :: Real, _ :: Real,
                 _ :: Real, _ :: Real])
  property (grad :: LinearGradient).stops
    :: List.of(matching([_ :: Real, _ :: Color]))
){

 A linear gradient for a @rhombus(Brush, ~class).

}

@doc(
  class RadialGradient():
    constructor (x0 :: Real, y0 :: Real, r0 :: Real,
                 x1 :: Real, y1 :: Real, r1 :: Real,
                 [[stop :: Real.in(0.0, 1.0), color :: Color], ...])
  property (grad :: RadialGradient).circles
    :: matching([_ :: Real, _ :: Real, _ :: Real,
                 _ :: Real, _ :: Real, _ :: Real])
  property (grad :: RadialGradient).stops
    :: List.of(matching([_ :: Real, _ :: Color]))
){

 A radial gradient for a @rhombus(Brush, ~class).

}
