#lang scribble/rhombus/manual
@(import:
    "common.rhm" open:
      except: Path
    meta_label:
      rhombus/draw:
        expose: Path)

@title{Path}

@doc(
  class Path()
){

 Creates a drawing path.

}

@doc(
  method (path :: Path).close() :: Void
  method (path :: Path).is_open() :: Boolean
  method (path :: Path).reset() :: Void
){

 Closes an open path, if any, check whether the path is currently open,
 or discards all points to reset the path.

}

@doc(
  method (path :: Path).move_to(pt :: PointLike)
    :: Void
  method (path :: Path).line_to(pt :: PointLike)
    :: Void
  method (path :: Path).curve_to(pt1 :: PointLike,
                                 pt2 :: PointLike,
                                 pt3 :: PointLike)
    :: Void
){

 Sets a starting poin for an open path, or extends an open path with a
 stright line or Bezier curve.

}


@doc(
  method (path :: Path).polygon([pt :: PointLike, ...],
                                ~dpt: dpt :: PointLike = Point.zero,
                                ~dx: dx :: Real = 0,
                                ~dy: dy :: Real = 0)
    :: Void
  method (path :: Path).rectangle(r :: RectLike)
    :: Void
  method (path :: Path).rounded_rectangle(r :: RectLike,
                                          radius :: Real = -0.25)
    :: Void
  method (path :: Path).ellipse(r :: RectLike)
    :: Void
  method (path :: Path).arc(r :: RectLike,
                            start :: Real, end :: Real,
                            ~clockwise: clockwise :: Any = #false)
    :: Void
){

 Adds to the path. If the path is currently open, it is first closed,
 except in the case of @rhombus(Path.arc).

}

@doc(
  method (path :: Path).scale(s :: Real) :: Void
  method (path :: Path).scale(sx :: Real, sy :: Real) :: Void
  method (path :: Path).rotate(radians :: Real) :: Void
){

 Adjusts a path to scale or rotate every point defining the path.

}

@doc(
  method (path :: Path).append(other_path :: Path) :: Void
){

 Adds @rhombus(other_path) to the end of @rhombus(path).

}

@doc(
  method (path :: Path).bounding_box() :: Rect
){

 Returns a rectangle that bounds all of the points describing
 @rhombus(path).

}
