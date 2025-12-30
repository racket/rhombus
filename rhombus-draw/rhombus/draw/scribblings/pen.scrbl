#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Pen}

A pen is typically installed to a drawing context's @rhombus(DC.pen)
property.

@doc(
  class draw.Pen():
    constructor (
      ~color: color :: (String || Color) = "Black",
      ~width: width :: Real.in(0, 255) = 1,
      ~cap: cap :: Pen.Cap = #'round,
      ~join: join :: Pen.Join = #'round,
      ~style: style :: Pen.Style = #'solid,
      ~stipple: stipple :: maybe(Bitmap) = #false,
    )
){

 Creates a pen configuration.

 A pen's ink straddles the mathematical line or curve that it draws, so
 that it spans @rhombus(width/2) drawing units on each side. A
 @rhombus(width) or @rhombus(0) is treated as a minimal drawin unit for
 the destination, such as a single pixel for a bitmap destination.

 A pen's @rhombus(cap) is used to shape each end of a line or curve,
 except in the case that multiple lines or curves are joined together. In
 the latter case, join points are shaped by @rhombus(cap).

 A pen's @rhombus(style) determines how it draws along a line or curve.
 Drawing with a @rhombus(#'transparent) pen is the same as not drawing.

 If a pen has a @rhombus(stipple) bitmap, some @rhombus(style)s are
 ignored (see @rhombus(Pen.Style)), and @rhombus(stipple) it is used to
 fill pixels that otherwise would be covered by the pen's ink. A
 monochrome stipple takes on @rhombus(color) as it is drawn.

 A pen like an existing one can be constructed using @rhombus(with) and
 the field names @rhombus(color, ~datum), @rhombus(width, ~datum),
 @rhombus(style, ~datum), @rhombus(cap, ~datum), @rhombus(join, ~datum),
 and/or @rhombus(stipple, ~datum).

}

@doc(
  property (pen :: draw.Pen).color :: Color
  property (pen :: draw.Pen).width :: Real.in(0, 255)
  property (pen :: draw.Pen).style :: Pen.Style
  property (pen :: draw.Pen).cap :: Pen.Cap
  property (pen :: draw.Pen).join :: Pen.Join
  property (pen :: draw.Pen).stipple :: maybe(Bitmap)
){

 Properties to access pen components.

}

@doc(
  enum draw.Pen.Style
  | transparent
  | solid
  | hilite
  | dot
  | long_dash
  | show_dash
  | dot_dash
  | xor_dot
  | xor_long_dot
  | xor_short_dot
  | xor_dot_dash
){

 Line-drawing modes. The @rhombus(#'transparent) mode applies an alpha
 of @rhombus(0) to drawing, @rhombus(#'solid) applies an alpha of
 @rhombus(1.0), and @rhombus(#'hilite) applies an alpha of @rhombus(0.3).
 The other modes describe a line shape, and they are treated like
 @rhombus(#'solid) for a pen with a stipple.

}

@doc(
  enum draw.Pen.Cap
  | round
  | projecting
  | butt
){

 Line-ending modes; see @rhombus(Pen).

}

@doc(
  enum draw.Pen.Join
  | round
  | bevel
  | miter
){

 Line-joining modes; see @rhombus(Pen).

}


@doc(
  def draw.Pen.none :: Pen
){

 A pen with style @rhombus(#'transparent).

}
