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
  method Path.close(path :: Path) :: Void
  method Path.is_open(path :: Path) :: Boolean
  method Path.reset(path :: Path) :: Void
){

 Closes an open path, if any, check whether the path is currently open,
 or discards all points to reset the path.

}

@doc(
  method Path.move_to(path :: Path, x :: Real, y :: Real)
  method Path.line_to(path :: Path, x :: Real, y :: Real)
  method Path.curve_to(path :: Path,
                       x1 :: Real, y1 :: Real,
                       x2 :: Real, y2 :: Real,
                       x3 :: Real, y3 :: Real)
){

 Sets a starting poin for an open path, or extends an open path with a
 stright line or Bezier curve.

}


@doc(
  method Path.polygon(path :: Path,
                      [[x :: Real, y :: Real], ...],
                      ~dx: dx :: Real = 0.0,
                      ~dy: dy :: Real = 0.0) :: Void
  method Path.rectangle(path :: Path,
                        x :: Real, y :: Real,
                        width :: Real.at_least(0.0),
                        height :: Real.at_least(0.0)) :: Void
  method Path.rounded_rectangle(path :: Path,
                                x :: Real, y :: Real,
                                width :: Real.at_least(0.0),
                                height :: Real.at_least(0.0),
                                radius :: Real = -0.25) :: Void
  method Path.ellipse(path :: Path,
                      x :: Real, y :: Real,
                      width :: Real.at_least(0.0),
                      height :: Real.at_least(0.0)) :: Void
  method Path.arc(path :: Path,
                  x :: Real, y :: Real,
                  width :: Real.at_least(0.0),
                  height :: Real.at_least(0.0),
                  start :: Real, end :: Real) :: Void
){

 Adds to the path. If the path is currently open, it it first closed.

}

@doc(
  method Path.scale(path :: Path, x :: Real, y :: Real) :: Void
  method Path.rotate(path :: Path, radians :: Real) :: Void
){

 Adjusts a path to scale or rotate every point defining the path.

}
