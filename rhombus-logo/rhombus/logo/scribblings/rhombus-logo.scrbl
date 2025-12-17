#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      rhombus/logo
      rhombus/draw
      pict)

@title(~category: #'#{gui-library}){Rhombus Logo}

@docmodule(rhombus/logo)

The Rhombus logo is also available as pre-rendered PNG and SVG images
from the @filepath{rhombus-icons} package.

@doc(
  fun logo.draw(dc :: draw.DC,
                ~dx: dx :: Real = 0,
                ~dy: dy :: Real = 0)
  def logo.size :: draw.Size
){

 Draws the Rhombus logo.

}


@doc(
  def logo.red :: draw.Color
  def logo.blue :: draw.Color
){

 Colors used in the Rhombus logo.

}


@doc(
  fun logo.save_png(path :: PathString,
                    ~as_square: as_square = #false)
  fun logo.save_svg(path :: PathString,
                    ~as_square: as_square = #false)
){

 Writes a rendered logo to a file.

}

@doc(
  fun logo.to_pict(dc :: ((draw.DC, Real, Real) -> ~any,
                          ~width: Real,
                          ~height: Real)
                     -> ~any)
){

 Intended to support the Rhombus logo in @rhombus(pict.Pict, ~annot) form using
 the @rhombusmodname(pict) library, but without creating a dependency on
 @rhombusmodname(pict): the @rhombus(pict.dc) function can be supplied as
 @rhombus(dc), and then the result is a @rhombus(pict.Pict, ~annot).

}
