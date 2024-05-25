#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Pen}

@doc(
  class Pen():
    constructor (
      ~color: color :: (String || Color) = "Black",
      ~width: width :: Real.in(0, 255) = 1,
      ~style: style :: Pen.Style = #'solid,
      ~cap: cap :: Pen.Cap = #'round,
      ~join: join :: Pen.Join = #'round,
      ~stipple: stipple :: maybe(Bitmap) = #false,
    )
){

 Creates a pen configuration.

 A pen like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(color), @rhombus(width), @rhombus(style),
 @rhombus(cap), @rhombus(join), and/or @rhombus(stipple).

}

@doc(
  property (pen :: Pen).color :: Color
  property (pen :: Pen).width :: Real.in(0, 255)
  property (pen :: Pen).style :: Pen.Style
  property (pen :: Pen).cap :: Pen.Cap
  property (pen :: Pen).join :: Pen.Join
  property (pen :: Pen).stipple :: maybe(Bitmap)
){

 Properties to access pen components.

}

@doc(
  enum Pen.Style:
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
  enum Pen.Cap:
    round
    projecting
    butt
){

 Line-ending modes.

}

@doc(
  enum Pen.Join:
    round
    bevel
    miter
){

 Line-joining modes.

}


@doc(
  def Pen.none :: Pen
){

 A pen with style @rhombus(#'transparent).

}
