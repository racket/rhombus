#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: Path
    meta_label:
      rhombus/draw:
        expose: Path)

@title{Region}

@doc(
  class draw.Region():
    constructor (dc :: maybe(draw.DC) = #false)
){

 Creates a region, optionally specific to @rhombus(dc).

}

@doc(
  property (rgn :: draw.Region).dc :: maybe(draw.DC)
){

 Reports the drawing context that the region is specific to, if any.

}

@doc(
  method (rgn :: draw.Region).is_empty() :: Boolean
  method (rgn :: draw.Region).contains(pt :: draw.PointLike) :: Boolean
){

  Queries the content represented by the region.

}

@doc(
  method (rgn :: draw.Region).polygon(
    [pt :: draw.PointLike, ...],
    ~dpt: dpt :: draw.PointLike = draw.Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: draw.Region.Fill = #'even_odd
  ) :: Void
  method (rgn :: draw.Region).rectangle(r :: draw.RectLike)
    :: Void
  method (rgn :: draw.Region).rounded_rectangle(r :: draw.RectLike,
                                                radius :: Real = -0.25)
    :: Void
  method (rgn :: draw.Region).ellipse(r :: draw.RectLike)
    :: Void
  method (rgn :: draw.Region).arc(r :: draw.RectLike,
                                  start :: Real, end :: Real)
    :: Void
  method (rgn :: draw.Region).path(
    p :: Path,
    ~dpt: dpt :: draw.PointLike = draw.Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: draw.Region.Fill = #'odd_even
  ) :: Void
){

 Adds to the region. A path or polygon is implicitly closed.

}

@doc(
  method (rgn :: draw.Region).union(rgn2 :: draw.Region) :: Void
  method (rgn :: draw.Region).intersect(rgn2 :: draw.Region) :: Void
  method (rgn :: draw.Region).subtract(rgn2 :: draw.Region) :: Void
  method (rgn :: draw.Region).xor(rgn2 :: draw.Region) :: Void
){

 Changes the region by applying a combination with another region. The
 other region, @rhombus(rgn2), is not modified.

}

@doc(
  enum draw.Region.Fill:
    odd_even
    winding
){

 A region polygon-fill mode.

}
