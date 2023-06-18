#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Pen}

@doc(
  class Pen(handle):
    constructor (~color: color :: (String || Color) = "Black",
                 ~width: width :: Real.in(0, 255) = 1,
                 ~style: style :: Pen.Style = #'solid,
                 ~cap: cap :: Pen.Cap = #'round,
                 ~join: join :: Pen.Join = #'round,
                 ~stipple: stipple :: maybe(Bitmap) = #false)
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
  annot.macro 'Pen.Style'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'transparent)}  
  @item{@rhombus(#'solid)}
  @item{@rhombus(#'xor)}
  @item{@rhombus(#'hilite)}
  @item{@rhombus(#'dot)}
  @item{@rhombus(#'long_dash)}
  @item{@rhombus(#'show_dash)}
  @item{@rhombus(#'dot_dash)}
  @item{@rhombus(#'xor_dot)}
  @item{@rhombus(#'xor_long_dot)}
  @item{@rhombus(#'xor_short_dot)}
  @item{@rhombus(#'xor_dot_dash)}
)

}

@doc(
  annot.macro 'Pen.Cap'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'round)}  
  @item{@rhombus(#'projecting)}
  @item{@rhombus(#'butt)}
)

}

@doc(
  annot.macro 'Pen.Join'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'round)}  
  @item{@rhombus(#'bevel)}
  @item{@rhombus(#'miter)}
)

}


@doc(
  def Pen.none :: Pen
){

 A pen with style @rhombus(#'transparent).

}
