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
  property (rgn :: Region).dc :: Maybe(DC)
){

 Reports the drawing context that the region is specific to, if any.

}

@doc(
  method (rgn :: Region).is_empty() :: Boolean
  method (rgn :: Region).contains(x :: Real, y :: Real) :: Boolean
){

  Queries the content represented by the region.

}

@doc(
  method (rgn :: Region).polygon([[x :: Real, y :: Real], ...],
                                 ~dx: dx :: Real = 0.0,
                                 ~dy: dy :: Real = 0.0,
                                 ~fill: fill :: Region.Fill = #'even_odd) :: Void
  method (rgn :: Region).rectangle(x :: Real, y :: Real,
                                   width :: Real.at_least(0.0),
                                   height :: Real.at_least(0.0)) :: Void
  method (rgn :: Region).rounded_rectangle(x :: Real, y :: Real,
                                           width :: Real.at_least(0.0),
                                           height :: Real.at_least(0.0),
                                           radius :: Real = -0.25) :: Void
  method (rgn :: Region).ellipse(x :: Real, y :: Real,
                                 width :: Real.at_least(0.0),
                                 height :: Real.at_least(0.0)) :: Void
  method (rgn :: Region).arc(x :: Real, y :: Real,
                             width :: Real.at_least(0.0),
                             height :: Real.at_least(0.0),
                             start :: Real, end :: Real) :: Void
  method (rgn :: Region).path(p :: Path,
                              ~dx: dx :: Real = 0.0,
                              ~dy: dy :: Real = 0.0,
                              ~fill: fill :: Region.Fill = #'odd_even) :: Void
){

 Adds to the region. A path or polygon is implicitly closed.

}

@doc(
  method (rgn :: Region).union(rgn2 :: Region) :: Void
  method (rgn :: Region).intersect(rgn2 :: Region) :: Void
  method (rgn :: Region).subtract(rgn2 :: Region) :: Void
  method (rgn :: Region).xor(rgn2 :: Region) :: Void
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
