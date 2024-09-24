#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Pen}

@doc(
  class draw.Pen():
    constructor (
      ~color: color :: (String || draw.Color) = "Black",
      ~width: width :: Real.in(0, 255) = 1,
      ~style: style :: draw.Pen.Style = #'solid,
      ~cap: cap :: draw.Pen.Cap = #'round,
      ~join: join :: draw.Pen.Join = #'round,
      ~stipple: stipple :: maybe(draw.Bitmap) = #false,
    )
){

 Creates a pen configuration.

 A pen like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(color), @rhombus(width), @rhombus(style),
 @rhombus(cap), @rhombus(join), and/or @rhombus(stipple).

}

@doc(
  property (pen :: draw.Pen).color :: draw.Color
  property (pen :: draw.Pen).width :: Real.in(0, 255)
  property (pen :: draw.Pen).style :: draw.Pen.Style
  property (pen :: draw.Pen).cap :: draw.Pen.Cap
  property (pen :: draw.Pen).join :: draw.Pen.Join
  property (pen :: draw.Pen).stipple :: maybe(draw.Bitmap)
){

 Properties to access pen components.

}

@doc(
  enum draw.Pen.Style:
    transparent
    solid
    xor
    hilite
    dot
    long_dash
    show_dash
    dot_dash
    xor_dot
    xor_long_dot
    xor_short_dot
    xor_dot_dash
){

 Line-drawing mode.

}

@doc(
  enum draw.Pen.Cap:
    round
    projecting
    butt
){

 Line-ending modes.

}

@doc(
  enum draw.Pen.Join:
    round
    bevel
    miter
){

 Line-joining modes.

}


@doc(
  def draw.Pen.none :: draw.Pen
){

 A pen with style @rhombus(#'transparent).

}
