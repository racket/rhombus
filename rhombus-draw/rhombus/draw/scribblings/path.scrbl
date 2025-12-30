#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Path}

@(~version_at_least "8.14.0.4")

@doc(
  class draw.Path()
){

 Creates a drawing path.

}

@doc(
  method (path :: draw.Path).close() :: Void
  method (path :: draw.Path).is_open() :: Boolean
  method (path :: draw.Path).reset() :: Void
){

 Closes an open path, if any, check whether the path is currently open,
 or discards all points to reset the path.

}

@doc(
  method (path :: draw.Path).move_to(pt :: PointLike)
    :: Void
  method (path :: draw.Path).line_to(pt :: PointLike)
    :: Void
  method (path :: draw.Path).curve_to(pt1 :: PointLike,
                                      pt2 :: PointLike,
                                      pt3 :: PointLike)
    :: Void
){

 Sets a starting point for an open path, or extends an open path with a
 straight line or Bezier curve.

}


@doc(
  method (path :: draw.Path).polygon(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0
  ) :: Void
  method (path :: draw.Path).rectangle(r :: RectLike)
    :: Void
  method (path :: draw.Path).rounded_rectangle(r :: RectLike,
                                               radius :: Real = -0.25)
    :: Void
  method (path :: draw.Path).ellipse(r :: RectLike)
    :: Void
  method (path :: draw.Path).arc(r :: RectLike,
                                 start :: Real, end :: Real,
                                 ~clockwise: clockwise :: Any = #false)
    :: Void
  method (path :: draw.Path).text_outline(
    str :: String,
    ~dpt: dpt :: Point = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~font: font :: Font = Font(),
    ~combine: combine :: DC.TextCombine = #'kern
  ) :: Void
){

 Adds to the path. If the path is currently open, it is first closed,
 except in the case of @rhombus(draw.Path.arc).

}

@doc(
  method (path :: draw.Path).scale(s :: Real) :: Void
  method (path :: draw.Path).scale(sx :: Real, sy :: Real) :: Void
  method (path :: draw.Path).rotate(radians :: Real) :: Void
  method (path :: draw.Path).translate(dx :: Real, dy :: Real) :: Void
  method (path :: draw.Path).transform(t :: Transformation) :: Void
){

 Adjusts a path to scale, rotate, translate, or transform every point defining the
 path.

}

@doc(
  method (path :: draw.Path).append(other_path :: draw.Path) :: Void
){

 Adds @rhombus(other_path) to the end of @rhombus(path).

}

@doc(
  method (path :: draw.Path).bounding_box() :: Rect
){

 Returns a rectangle that bounds all of the points describing
 @rhombus(path).

}
