#lang scribble/rhombus/manual
@(import:
    "common.rhm" open:
      except: Path
    meta_label:
      rhombus/draw:
        expose: Path)

@title{Region}

@doc(
  class Region(handle):
    constructor (dc :: Maybe(DC) = #false)
){

 Creates a region, optionally specific to @rhombus(dc).

}

@doc(
  property Region.dc(rgn :: Region) :: Maybe(DC)
){

 Reports the drawing context that the region is specific to, if any.

}

@doc(
  method Region.is_empty(rgn :: Region) :: Boolean
  method Region.contains(rgn :: Region, x :: Real, y :: Real) :: Boolean
){

  Queries the content represented by the region.

}

@doc(
  method Region.polygon(rgn :: Region,
                        [[x :: Real, y :: Real], ...],
                        ~dx: dx :: Real = 0.0,
                        ~dy: dy :: Real = 0.0,
                        ~fill: fill :: Region.Fill = #'even_odd) :: Void
  method Region.rectangle(rgn :: Region,
                          x :: Real, y :: Real,
                          width :: Real.at_least(0.0),
                          height :: Real.at_least(0.0)) :: Void
  method Region.rounded_rectangle(rgn :: Region,
                                  x :: Real, y :: Real,
                                  width :: Real.at_least(0.0),
                                  height :: Real.at_least(0.0),
                                  radius :: Real = -0.25) :: Void
  method Region.ellipse(rgn :: Region,
                        x :: Real, y :: Real,
                        width :: Real.at_least(0.0),
                        height :: Real.at_least(0.0)) :: Void
  method Region.arc(rgn :: Region,
                    x :: Real, y :: Real,
                    width :: Real.at_least(0.0),
                    height :: Real.at_least(0.0),
                    start :: Real, end :: Real) :: Void
  method Region.path(rgn :: Region,
                     p :: Path,
                     ~dx: dx :: Real = 0.0,
                     ~dy: dy :: Real = 0.0,
                     ~fill: fill :: Region.Fill = #'odd_even) :: Void
){

 Adds to the region. A path or polygon is implicitly closed.

}

@doc(
  method Region.union(rgn :: Region, rgn2 :: Region) :: Void
  method Region.intersect(rgn :: Region, rgn2 :: Region) :: Void
  method Region.subtract(rgn :: Region, rgn2 :: Region) :: Void
  method Region.xor(rgn :: Region, rgn2 :: Region) :: Void
){

 Changes the region by applying a combination with another region. The
 other region, @rhombus(rgn2), is not modified.

}

@doc(
  annot.macro 'Region.Fill'
){

 Satisfied by the following symbols:

@itemlist(
  @item{@rhombus(#'even_odd)}  
  @item{@rhombus(#'winding)}
)

}
