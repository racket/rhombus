#lang scribble/rhombus/manual
@(import:
    "common.rhm" open:
      except: Path
    meta_label:
      rhombus/draw:
        expose: Path)

@title{Path}

@doc(
  class Path(handle):
    constructor ()
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
  method (path :: Path).move_to(x :: Real, y :: Real)
  method (path :: Path).line_to(x :: Real, y :: Real)
  method (path :: Path).curve_to(x1 :: Real, y1 :: Real,
                                 x2 :: Real, y2 :: Real,
                                 x3 :: Real, y3 :: Real)
){

 Sets a starting poin for an open path, or extends an open path with a
 stright line or Bezier curve.

}


@doc(
  method (path :: Path).polygon([[x :: Real, y :: Real], ...],
                                ~dx: dx :: Real = 0.0,
                                ~dy: dy :: Real = 0.0) :: Void
  method (path :: Path).rectangle(x :: Real, y :: Real,
                                  width :: NonnegReal,
                                  height :: NonnegReal) :: Void
  method (path :: Path).rounded_rectangle(x :: Real, y :: Real,
                                          width :: NonnegReal,
                                          height :: NonnegReal,
                                          radius :: Real = -0.25) :: Void
  method (path :: Path).ellipse(x :: Real, y :: Real,
                                width :: NonnegReal,
                                height :: NonnegReal) :: Void
  method (path :: Path).arc(x :: Real, y :: Real,
                            width :: NonnegReal,
                            height :: NonnegReal,
                            start :: Real, end :: Real) :: Void
){

 Adds to the path. If the path is currently open, it it first closed.

}

@doc(
  method (path :: Path).scale(x :: Real, y :: Real) :: Void
  method (path :: Path).rotate(radians :: Real) :: Void
){

 Adjusts a path to scale or rotate every point defining the path.

}
