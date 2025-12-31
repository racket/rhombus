#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Path}

A @rhombus(Path, ~annot) represents a set of figures defined by curves.
A path can be used with the @rhombus(DC.path) method of a drawing
context to draw the path's curves as lines, fill the region bounded by
the path's curves, or both. A path can also be used with the
@rhombus(Region.path) method to generate a region bounded by the path's
curves.

A path consists of zero or more @deftech{closed subpaths}, and possibly
one @deftech{open subpath}. Some @rhombus(Path) methods extend the open
subpath, some methods close the open subpath, and some methods add
closed subpaths. This approach to drawing formulation is inherited from
PostScript.

When a path is drawn as a line, a closed subpath is drawn as a closed
figure, analogous to a polygon. An open subpath is drawn with disjoint
start and end points, analogous lines drawn with @rhombus(DC.lines).

When a path is filled or used as a region, the open subpath (if any) is
treated as if it were closed. The content of a path is determined either
through the @rhombus(#'even_odd) rule or the @rhombus(#'winding) rule,
as selected at the time when the path is filled or used to generate a
region.

A path is not connected to any particular @rhombus(DC, ~annot) object,
so setting a @rhombus(DC, ~annot) transformation does not affect path
operations. Instead, a @rhombus(DC, ~annot)'s transformation applies at
the time that the path is drawn or used to set a region.

@doc(
  class draw.Path()
){

 Creates an empty set of drawing paths.

}

@doc(
  method (path :: draw.Path).close() :: Void
  method (path :: draw.Path).is_open() :: Boolean
  method (path :: draw.Path).reset() :: Void
){

 Closes an @tech{open subpath}, if any, check whether a subpath is
 currently open, or discards all points to reset the path.

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

 Sets a starting point for an @tech{open subpath}, or extends an open
 path with a straight line or Bezier curve.

}

@doc(
  method (path :: draw.Path).lines(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0
  ) :: Void
){

 Adds multiple lines to an open path in the same way as
 @rhombus(Path.line_to). Each point is shifted by @rhombus(dpt),
 @rhombus(dx), and @rhombus(dy).

}


@doc(
  method (path :: draw.Path).polygon(
    [pt :: PointLike, ...],
    ~dpt: dpt :: PointLike = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0
  ) :: Void
){

 Closes an @tech{open subpath}, if any, adds lines to a new subpath like
 @rhombus(Path.lines), and then closes the open subpath.

}

@doc(
  method (path :: draw.Path).rectangle(r :: RectLike)
    :: Void
){

 Closes the @tech{open subpath}, if any, and adds a @tech{closed subpath}
 that represents a rectangle @rhombus(r). (This convenience method is
 implemented in terms of @rhombus(Path.close), @rhombus(Path.move_to),
 and @rhombus(Path.line_to).)

}

@doc(
  method (path :: draw.Path).rounded_rectangle(
    r :: RectLike,
    radius :: Real = -0.25
  ) :: Void
){

 Closes the @tech{open subpath}, if any, and adds a @tech{closed subpath}
 that represents a round-cornered rectangle bound by @rhombus(r). (This
 convenience method is implemented in terms of @rhombus(Path.close),
 @rhombus(Path.move_to), @rhombus(Path.line_to), and @rhombus(Path.arc).)

 If @rhombus(radius) is positive, the value is used as the radius of the
 rounded corner. If @rhombus(radius) is negative, the absolute value is
 used as the proportion of the smallest dimension of the rectangle.

 If @rhombus(radius) is less than @rhombus(-0.5) or more than half of
 the width or height of @rhombus(r), an exception is thrown.

}

@doc(
  method (path :: draw.Path).ellipse(
    r :: RectLike
  ) :: Void
){

 Closes the @tech{open subpath}, if any, and adds a @tech{closed subpath}
 that represents an ellipse bounded by @rhombus(r). (This convenience
 method is implemented in terms of @rhombus(Path.close),
 @rhombus(Path.move_to), and @rhombus(Path.arc).)

}

@doc(
  method (path :: draw.Path).arc(
    r :: RectLike,
    start_radians :: Real,
    end_radians :: Real,
    ~clockwise: clockwise :: Any = #false
  ) :: Void
){

 Extends or starts the path’s @tech{open subpath} with a curve that
 corresponds to a section of an ellipse bounded by @rhombus(r). The
 ellipse section starts at the angle @rhombus(start_radians) (0 is three
 o’clock and @rhombus(math.pi/2) is twelve o’clock) and continues to the
 angle @rhombus(end_radians). If @rhombus(clockwise) is true, then the
 arc runs clockwise from @rhombus(start_radians) to
 @rhombus(end_radians), otherwise it runs clockwise.

 If the path has no open subpath, a new one is started with the arc’s
 starting point. Otherwise, the arc extends the existing sub-path, and
 the existing path is connected with a line to the arc’s starting point.

}

@doc(
  method (path :: draw.Path).text_outline(
    str :: String,
    ~dpt: dpt :: Point = Point.zero,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~font: font :: Font = Font(),
    ~combine: combine :: DC.TextCombine = #'kern
  ) :: Void
){

 Closes the @tech{open subpath}, if any, and adds a @tech{closed
  subpath} to outline @rhombus(str) using @rhombus(font). The top left of
 the text is positioned at @rhombus(dpt) plus @rhombus(dx) and
 @rhombus(dy). The @rhombus(combine) argument enables kerning and
 character combination; see @rhombus(DC.TextCombine, ~annot).

}

@doc(
  method (path :: draw.Path).scale(s :: Real) :: Void
  method (path :: draw.Path).scale(sx :: Real, sy :: Real) :: Void
  method (path :: draw.Path).rotate(radians :: Real) :: Void
  method (path :: draw.Path).translate(dx :: Real, dy :: Real) :: Void
  method (path :: draw.Path).transform(t :: Transformation) :: Void
){

 Adjusts a path to scale, rotate, translate, or transform every point
 defining the path.

}

@doc(
  method (path :: draw.Path).append(other_path :: draw.Path) :: Void
){

 Adds @rhombus(other_path) to the end of @rhombus(path).

 @tech{Closed subpaths} of path are added as @tech{closed subpaths} to
 @rhombus(path). If both paths have an open subpath, then this path’s
 open subpath is extended by the given path’s open subpath, adding a line
 from this path’s current ending point to the given path’s starting
 point. If only one of the paths has an open subpath, then it becomes (or
 remains) the path’s open subpath.

}

@doc(
  method (path :: draw.Path).reverse() :: Void
){

 Reverses the direction of all subpaths within @rhombus(path). If the
 path has an @tech{open subpath}, the starting point becomes the ending
 point, and extensions to the open sub-path build on this new ending
 point. Reversing a @tech{closed subpath} affects how it combines with
 other subpaths when determining the content of a path in
 @rhombus(#'winding) mode.

}

@doc(
  method (path :: draw.Path).bounding_box() :: Rect
){

 Returns a rectangle that bounds all of the points describing
 @rhombus(path).

}

@doc(
  property (path :: draw.Path).handle :: Any
  fun draw.Path.from_handle(hand :: Any) :: Path
){

 The @rhombus(Path.handle) property returns a Racket object that
 corresponds to the path for use directly with
 @racketmodname(racket/draw). The @rhombus(Path.from_handle) function
 creates a @rhombus(Path, ~class) from such a Racket object.

}
