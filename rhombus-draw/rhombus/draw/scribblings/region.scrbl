#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: Path
    meta_label:
      rhombus/draw:
        expose: Path)

@title{Region}

A @rhombus(Region, ~annot) object specifies a portion of a drawing area
(possibly discontinuous). It is normally used for clipping drawing
operations.

@doc(
  class draw.Region():
    constructor (dc :: maybe(DC) = #false)
){

 A @rhombus(Region, ~annot) object can be associated to a particular
 @rhombus(DC, ~annot) object when the region is created. In that case,
 the region uses the drawing context's current transformation matrix,
 translation, scaling, and rotation, independent of the transformation
 that is in place when the region is installed. Otherwise, the region is
 transformed as usual when it is installed into a @rhombus(DC, ~annot).
 For an auto-scrolled canvas, the canvas's current scrolling always
 applies when the region is used (and it does not affect the region’s
 bounding box).

 Region combination with operations like @rhombus(Rhombus.union) are
 implemented by combining paths. Certain combinations work only if the
 paths have a suitable fill mode, which can be either
 @rhombus(#'winding), @rhombus(#'even_odd), or a @deftech{flexible fill}
 mode. When a region is installed as a device context's clipping region,
 any subpath with a flexible fill mode uses @rhombus(#'even_odd) mode if
 any other path uses @rhombus(#'even_odd) mode.

}

@doc(
  property (rgn :: draw.Region).dc :: maybe(DC)
){

 Reports the drawing context that the region is specific to, if any.

}

@doc(
  method (rgn :: draw.Region).is_empty() :: Boolean
  method (rgn :: draw.Region).contains(pt :: PointLike) :: Boolean
){

  Queries the content represented by the region.

}

@doc(
  method (rgn :: draw.Region).polygon(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: Region.Fill = #'even_odd
  ) :: Void
){

 Sets the region to the interior of the polygon specified by
 @rhombus(pt)s. Them @rhombus(fill) style determines how multiple loops
 are handled in the same way as @rhombus(DC.polygon).

 The @rhombus(fill) style affects how well the region reliably combines
 with other regions via @rhombus(Region.union), @rhombus(Region.xor), and
 @rhombus(Region.subtract). The region is also atomic for the purposes of
 region combination.

}

@doc(
  method (rgn :: draw.Region).rectangle(r :: RectLike)
    :: Void
){

 Sets the region to the interior of the specified rectangle. See also
 @rhombus(DC.rectangle), since the region content is determined the same
 way as brush-based filling in a @rhombus(DC, ~annot).

 The region corresponds to a clockwise path with a @tech{flexible fill}.
 The region is also atomic for the purposes of region combination.

}

@doc(
  method (rgn :: draw.Region).rounded_rectangle(r :: RectLike,
                                                radius :: Real = -0.25)
    :: Void
  ){

 Sets the region to the interior of the specified rounded rectangle. See
 also @rhombus(DC.rounded_rectangle), since the region content is
 determined the same way as brush-based filling in a
 @rhombus(DC, ~annot).

 The region corresponds to a clockwise path with a @tech{flexible fill}.
 The region is also atomic for the purposes of region combination.

}

@doc(
  method (rgn :: draw.Region).ellipse(r :: RectLike)
    :: Void
){

 Sets the region to the interior of an ellipse bounded by the specified
 rectangle. See also @rhombus(DC.ellipse), since the region content is
 determined the same way as brush-based filling in a
 @rhombus(DC, ~annot).

 The region corresponds to a clockwise path with a @tech{flexible fill}.
 The region is also atomic for the purposes of region combination.

}

@doc(
  method (rgn :: draw.Region).arc(r :: RectLike,
                                  start :: Real, end :: Real)
    :: Void
){

 Sets the region to the interior of a wedged form by the specified arc.
 See also @rhombus(DC.arc), since the region content is determined the
 same way as brush-based filling in a @rhombus(DC, ~annot).

 The region corresponds to a clockwise path with a @tech{flexible fill}.
 The region is also atomic for the purposes of region combination.

}


@doc(
  method (rgn :: draw.Region).path(
    p :: draw.Path,
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~fill: fill :: Region.Fill = #'odd_even
  ) :: Void
){

 Sets the region's path to the given path. Them @rhombus(fill) style
 determines how multiple loops are handled in the same way as
 @rhombus(Region.polygon). See also @rhombus(DC.path), since the region
 content is determined the same way as brush-based filling in a
 @rhombus(DC, ~annot).

 The @rhombus(fill) style affects how well the region reliably combines
 with other regions via @rhombus(Region.union), @rhombus(Region.xor), and
 @rhombus(Region.subtract). The region is also atomic for the purposes of
 region combination.

}

@doc(
  method (rgn :: draw.Region).union(rgn2 :: Region) :: Void
  method (rgn :: draw.Region).intersect(rgn2 :: Region) :: Void
  method (rgn :: draw.Region).subtract(rgn2 :: Region) :: Void
  method (rgn :: draw.Region).xor(rgn2 :: Region) :: Void
){

 Changes the region by applying a combination with another region. The
 other region, @rhombus(rgn2), is not modified. The drawing context of
 @rhombus(rgn) and @rhombus(rgn2) must be the same, or they must both be
 unassociated to any drawing context.

 A @rhombus(Region.intersect) corresponds to clipping with
 @rhombus(rgn)'s path, and then clipping with @rhombus(rgn2)’s path.
 Further combining sends to this region correspond to combination with
 the original path before initial clip, and further combination with this
 region as an argument correspond to a combination with the given path
 after the initial clip. Thus, an intersected region is a poor input for
 @rhombus(Region.union), @rhombus(Region.subtract), or
 @rhombus(Region.xor), but it intersects properly in further calls to
 @rhombus(Region.intersect).

 A @rhombus(Region.union) corresponds to combining the subpaths of each
 region into one path, using an @rhombus(#'odd_even) fill if either of
 the region uses an @rhombus(#'odd_even) fill (otherwise using a
 @rhombus(#'winding) fill), a @rhombus(#'winding) fill if either region
 uses a @rhombus(#'winding) fill, or the fill remains a @tech{flexible
  fill} if both paths have a flexible fill. Consequently, while the result
 is consistent across platforms and devices, it is a true union only for
 certain input regions. For example, it is a true union for
 non-overlapping atomic and union regions. It is also a true union for
 atomic and union regions (potentially overlapping) that are all
 clockwise and use @rhombus(#'winding) fill or if the fills are all
 flexible fills.

 A @rhombus(Region.subtract) corresponds to combining the subpaths of
 @rhombus(rgn) region with the reversed subpaths of @rhombus(rgn2), then
 intersecting the result with @rhombus(rgn). This fails as a true
 subtraction, because the boundary of loops (with either
 @rhombus(#'odd_even) or @rhombus(#'winding) filling) is ambiguous.

 A @rhombus(Region.xor) corresponds to combining the subpaths of
 @rhombus(rgn) with the reversed subpaths of @rhombus(rgn2). The result
 uses an @rhombus(#'odd_even) fill if either of the region uses an
 @rhombus(#'odd_even) fill, a @rhombus(#'winding) fill in either region
 uses a winding fill, or the fill remains a flexible fill if both paths
 have a flexible fill. Consequently, the result is a reliable xoring only
 for certain input regions. For example, it is reliable for atomic and
 xoring regions that all use @rhombus(#'odd_even) fill.

}

@doc(
  enum draw.Region.Fill
  | odd_even
  | winding
){

 A region polygon-fill mode.

}

@doc(
  property (path :: draw.Region).handle :: Any
  fun draw.Region.from_handle(hand :: Any) :: Region
){

 The @rhombus(Region.handle) property returns a Racket object that
 corresponds to the region for use directly with
 @racketmodname(racket/draw). The @rhombus(Region.from_handle) function
 creates a @rhombus(Region, ~class) from such a Racket object.

}
