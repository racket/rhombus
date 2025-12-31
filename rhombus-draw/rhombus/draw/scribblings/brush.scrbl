#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Brush}

A brush is typically installed to a drawing context's @rhombus(DC.brush)
property.

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

 A brush's ink fills pixels that are bounded by a set of lines or
 curves. The brush's @rhombus(style) describes the shape that is repeated
 to fill a region of pixels.

 If a brush has a @rhombus(stipple) bitmap, then some @rhombus(style)s
 are ignored (see @rhombus(Brush.Style)), and @rhombus(stipple) it is
 used to fill pixels that otherwise would be covered by the brush's ink.
 A monochrome stipple takes on @rhombus(color) as it is drawn.

 If a brush as a @rhombus(gradient), then @rhombus(color),
 @rhombus(style), and @rhombus(stipple) are ignored. With a
 @rhombus(gradient), for each point in a drawing destination, the
 gradient associates a color to the point based on starting and ending
 colors and starting and ending lines (for a linear gradient) or circles
 (for a radial gradient); a gradient-assigned color is applied for each
 point that is touched when drawing with the brush.

 A brush like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(color, ~datum), @rhombus(style, ~datum),
 @rhombus(stipple, ~datum), and/or @rhombus(gradient, ~datum).

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
  enum draw.Brush.Style
  | transparent
  | solid
  | opaque
  | hilite
  | bdiagonal_hatch
  | crossdiag_hatch
  | fdiagonal_hatch
  | cross_hatch
  | horizontal_hatch
  | vertical_hatch
){

 Brush-filling modes. The @rhombus(#'transparent) mode applies an alpha
 of @rhombus(0) to drawing, @rhombus(#'solid) applies an alpha of
 @rhombus(1.0), and @rhombus(#'hilite) applies an alpha of @rhombus(0.3).
 The @rhombus(#'opaque) mode is the same as @rhombus(#'solid) for a brush
 without a stipple, but it causes a monochome stipple to use the
 destination's background color in place of white pixels within the
 stipple. The other modes describe a brush pattern, and they are treated
 like @rhombus(#'solid) for a brush with a stipple.

}


@doc(
  def draw.Brush.none :: Brush
){

 A brush with style @rhombus(#'transparent).

}

@doc(
  class draw.LinearGradient():
    constructor (pt0 :: PointLike,
                 pt1 :: PointLike,
                 [[stop :: Real.in(0.0, 1.0), color :: Color], ...])
  property (grad :: draw.LinearGradient).line
    :: [Point, _ :: Point]
  property (grad :: draw.LinearGradient).stops
    :: List.of([Real.in(0.0, 1.0), Color])
){

 A linear gradient for a @rhombus(Brush, ~class) used to fill areas with
 smooth color transitions.

 Color transitions are based on a line, where colors are assigned to stop
 points along the line, and colors for in-between points are interpolated
 from the stop-point colors. The color of a point on the gradient’s line
 is propagated to all points in the drawing context that are touched by a
 line through the point and perpendicular to the gradient's line.

 The line is from @rhombus(pt0) to @rhombus(pt1). The stops list assigns
 colors to stop points along the line, where @rhombus(0.0) corresponds to
 @rhombus(pt0), @rhombus(1.0) corresponds to @rhombus(pt1), and numbers
 in between correspond to points in between.

 Elements in stops are implicitly sorted by point (i.e., by the number
 between @rhombus(0.0) and @rhombus(1.0)). Order is preserved for
 multiple elements for the same point, in which case the first element
 for a given point is treated infinitesimally before the point, and
 additional elements between the first and last for a stop point are
 effectively ignored.

}

@doc(
  class draw.RadialGradient():
    constructor ([[pt0 :: PointLike], r0 :: Real],
                 [[pt1 :: PointLike], r1 :: Real],
                 [[stop :: Real.in(0.0, 1.0), color :: Color], ...])
  property (grad :: draw.RadialGradient).circles
    :: [[PointLike, Real],
        [PointLike, Real]]
  property (grad :: draw.RadialGradient).stops
    :: List.of([Real.in(0.0, 1.0), Color])
){

 A radial gradient for a @rhombus(Brush, ~class) used to fill areas with
 smooth color transitions.

 Color transitions are based on two circles and the sequence of circles
 that ``morph'' from the starting circle to the ending circle. Normally,
 one of the two circles defining a gradient is nested within the other;
 in that case, points within the inner circle get the same color as the
 inner circle's edge, while points outside the outer circle get the same
 color as the outer circle's edge.

 Creates a radial gradient with the starting circle as the one with
 radius @rhombus(r0) centered at @rhombus(pt0) and the ending circle as
 the one with radius @rhombus(r1) centered at @rhombus(pt1). The stops
 list assigns colors to circles, where @rhombus(0.0) corresponds to the
 starting circle, @rhombus(1.0) corresponds to the ending circle, and
 numbers in between correspond to circles in between.

 The order of elements within stops and duplicate points are treated in
 the same way for as @rhombus(LinearGradient).

}

@doc(
  property (path :: draw.Brush).handle :: Any
  fun draw.Brush.from_handle(hand :: Any) :: Brush
){

 The @rhombus(Brush.handle) property returns a Racket object that
 corresponds to the brush for use directly with
 @racketmodname(racket/draw). The @rhombus(Brush.from_handle) function
 creates a @rhombus(Brush, ~class) from such a Racket object.

}
