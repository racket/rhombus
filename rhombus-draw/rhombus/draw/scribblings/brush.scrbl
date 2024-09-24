#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Brush}

@doc(
  class draw.Brush():
    constructor (
      ~color: color :: (String || draw.Color) = "Black",
      ~style: style :: draw.Brush.Style = #'solid,
      ~stipple: stipple :: maybe(draw.Bitmap) = #false,
      ~gradient:
        gradient :: maybe(draw.LinearGradient || draw.RadialGradient):
          #false,
    )
){

 Creates a brush configuration.

 A brush like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(color), @rhombus(style), @rhombus(stipple),
 and/or @rhombus(gradient).

}

@doc(
  property (brush :: draw.Brush).color :: draw.Color
  property (brush :: draw.Brush).style :: draw.Brush.Style
  property (brush :: draw.Brush).stipple :: maybe(draw.Bitmap)
  property (brush :: draw.Brush).gradient
    :: maybe(draw.LinearGradient || draw.RadialGradient)
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
  def draw.Brush.none :: draw.Brush
){

 A brush with style @rhombus(#'transparent).

}

@doc(
  class draw.LinearGradient():
    constructor (pt1 :: draw.PointLike,
                 pt2 :: draw.PointLike,
                 [[stop :: Real.in(0.0, 1.0), color :: draw.Color], ...])
  property (grad :: draw.LinearGradient).line
    :: matching([_ :: draw.Point, _ :: draw.Point])
  property (grad :: draw.LinearGradient).stops
    :: List.of(matching([_ :: Real.in(0.0, 1.0), _ :: draw.Color]))
){

 A linear gradient for a @rhombus(draw.Brush, ~class).

}

@doc(
  class draw.RadialGradient():
    constructor ([[pt1 :: draw.PointLike], r1 :: Real],
                 [[pt2 :: draw.PointLike], r2 :: Real],
                 [[stop :: Real.in(0.0, 1.0), color :: draw.Color], ...])
  property (grad :: draw.RadialGradient).circles
    :: matching([[_ :: draw.PointLike, _ :: Real],
                 [_ :: draw.PointLike, _ :: Real]])
  property (grad :: draw.RadialGradient).stops
    :: List.of(matching([_ :: Real.in(0.0, 1.0), _ :: draw.Color]))
){

 A radial gradient for a @rhombus(draw.Brush, ~class).

}
