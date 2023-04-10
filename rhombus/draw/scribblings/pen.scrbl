#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Pen}

@doc(
  class Pen(handle):
    constructor (~like: like :: Maybe(Pen) = #false,
                 ~color: color :: (String || Color) = "Black",
                 ~width: width :: Real.in(0, 255) = 1,
                 ~style: style :: Pen.Style = #'solid,
                 ~cap: cap :: Pen.Cap = #'round,
                 ~join: join :: Pen.Join = #'round,
                 ~stipple: stipple :: Maybe(Bitmap) = #false)
){

 Creates a pen configuration.

 If @rhombus(like) is provided as a @rhombus(Font), then @rhombus(like)
 provides default values for other arguments, instead of the normal
 defaults.

}

@doc(
  property Pen.color(pen :: Pen) :: Color
  property Pen.width(pen :: Pen) :: Real.in(0, 255)
  property Pen.style(pen :: Pen) :: Pen.Style
  property Pen.cap(pen :: Pen) :: Pen.Cap
  property Pen.join(pen :: Pen) :: Pen.Join
  property Pen.stipple(pen :: Pen) :: Maybe(Bitmap)
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
