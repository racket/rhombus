#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Brush}

@doc(
  class draw.Brush():
    constructor (
      ~color: color :: (String || Color) = "Black",
      ~style: style :: Brush.Style = #'solid,
      ~stipple: stipple :: maybe(Bitmap) = #false,
      ~gradient: gradient :: maybe(LinearGradient || RadialGradient) = #false
    )
){

 Creates a brush configuration.

 A brush like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(color), @rhombus(style), @rhombus(stipple),
 and/or @rhombus(gradient).

}

@doc(
  property (brush :: draw.Brush).color :: Color
  property (brush :: draw.Brush).style :: Brush.Style
  property (brush :: draw.Brush).stipple :: maybe(Bitmap)
  property (brush :: draw.Brush).gradient
    :: maybe(LinearGradient || RadialGradient)
){

 Properties to access brush components.

}

@doc(
  enum draw.Brush.Style:
    transparent
    solid
    opaque
    xor
    hilite
    panel
    bdiagonal_hatch
    crossdiag_hatch
    fdiagonal_hatch
    cross_hatch
    horizontal_hatch
    vertical_hatch
){

 Brush-filling mode.

}


@doc(
  def draw.Brush.none :: Brush
){

 A brush with style @rhombus(#'transparent).

}

@doc(
  class draw.LinearGradient():
    constructor (pt1 :: PointLike,
                 pt2 :: PointLike,
                 [[stop :: Real.in(0.0, 1.0), color :: Color], ...])
  property (grad :: LinearGradient).line
    :: matching([_ :: Point, _ :: Point])
  property (grad :: LinearGradient).stops
    :: List.of(matching([_ :: Real.in(0.0, 1.0), _ :: Color]))
){

 A linear gradient for a @rhombus(Brush, ~class).

}

@doc(
  class draw.RadialGradient():
    constructor ([[pt1 :: PointLike], r1 :: Real],
                 [[pt2 :: PointLike], r2 :: Real],
                 [[stop :: Real.in(0.0, 1.0), color :: Color], ...])
  property (grad :: RadialGradient).circles
    :: matching([[_ :: PointLike, _ :: Real],
                 [_ :: PointLike, _ :: Real]])
  property (grad :: RadialGradient).stops
    :: List.of(matching([_ :: Real.in(0.0, 1.0), _ :: Color]))
){

 A radial gradient for a @rhombus(Brush, ~class).

}
