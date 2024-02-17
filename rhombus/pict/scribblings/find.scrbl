#lang scribble/rhombus/manual

@(import:
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "find"){Pict Finders}

@doc(
  annot.macro 'Find'
){

 Satisfied by a @deftech{finder}, which is applied to a pict to obtain
 two numbers representing an x-offset and y-offset.

}

@doc(
  fun Find(
    pict :: Pict,
    ~horiz: horiz :: HorizAlignment = #'center,
    ~vert: vert :: VertAlignment = #'center,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~dhoriz: dhoriz :: Real = 0,
    ~dvert: dvert :: Real = 0
  ) :: Find
){

 Creates a @tech{finder} that locates a @rhombus(pict) within another
 @tech{pict}. The @rhombus(Find.in) method takes the pict that contains
 @rhombus(pict) and returns two numbers, the x-offset and y-offset.

 The @rhombus(horiz) and @rhombus(vert) arguments determine the position
 within @rhombus(pict) that is fount. For example, the combination
 @rhombus(#'right) and @rhombus(#'bottom) find the bottom-right corner of
 @rhombus(pict)'s bounding box within another pict.

 The @rhombus(dx) and @rhombus(dy) offsets are added to an x-offset and
 y-offset that would be returned otherwise.

 The @rhombus(dhoriz) and @rhombus(dvert) arguments are multipled by the
 height and width, respectively, of the pict as located within another
 pict (which can be different than @rhombus(pict)'s immediate dimensions
 due to scaling). The multipled values are then added to an x-offset and
 y-offset that would be returned otherwise, in addition to @rhombus(dx)
 and @rhombus(dy).

}

@doc(
  fun Find.abs(~dx: dx :: Real = 0, -dy: dy :: Real = 0) :: Find
){

 Creates a @tech{finder} that always returns @rhombus(dx) and
 @rhombus(dy) without needing to locate any particular component
 @tech{pict}.

}


@doc(
  method (finder :: Find).in(pict :: Pict) :: values(Real, Real)
){

 Applies @rhombus(finder) to @rhombus(pict). An exception is thrown is a
 needed component pict cannot be found in @rhombus(pict).

 If @rhombus(pict) is an animated picture, then the search corresponds
 to finding within @rhombus(Pict.snapshot(pict)). When a finder for an
 animated pict is provdied to a function like @rhombus(pin), however,
 @rhombus(pin) will produce an animated pict where the finder is used
 separately for each snapshot generated from the combined animated pict.

}


@doc(
  fun Find.center(pict :: Pict, ....) :: Find
  fun Find.left(pict :: Pict, ....) :: Find
  fun Find.right(pict :: Pict, ....) :: Find
  fun Find.top(pict :: Pict, ....) :: Find
  fun Find.topline(pict :: Pict, ....) :: Find
  fun Find.baseline(pict :: Pict, ....) :: Find
  fun Find.bottom(pict :: Pict, ....) :: Find
  fun Find.left_top(pict :: Pict, ....) :: Find
  fun Find.left_topline(pict :: Pict, ....) :: Find
  fun Find.left_center(pict :: Pict, ....) :: Find
  fun Find.left_baseline(pict :: Pict, ....) :: Find
  fun Find.left_bottom(pict :: Pict, ....) :: Find
  fun Find.center_top(pict :: Pict, ....) :: Find
  fun Find.center_topline(pict :: Pict, ....) :: Find
  fun Find.center_center(pict :: Pict, ....) :: Find
  fun Find.center_baseline(pict :: Pict, ....) :: Find
  fun Find.center_bottom(pict :: Pict, ....) :: Find
  fun Find.right_top(pict :: Pict, ....) :: Find
  fun Find.right_topline(pict :: Pict, ....) :: Find
  fun Find.right_center(pict :: Pict, ....) :: Find
  fun Find.right_baseline(pict :: Pict, ....) :: Find
  fun Find.right_bottom(pict :: Pict, ....) :: Find
  fun Find.top_left(pict :: Pict, ....) :: Find
  fun Find.top_center(pict :: Pict, ....) :: Find
  fun Find.top_right(pict :: Pict, ....) :: Find
  fun Find.topline_left(pict :: Pict, ....) :: Find
  fun Find.topline_center(pict :: Pict, ....) :: Find
  fun Find.center_left(pict :: Pict, ....) :: Find
  fun Find.center_right(pict :: Pict, ....) :: Find
  fun Find.baseline_left(pict :: Pict, ....) :: Find
  fun Find.baseline_center(pict :: Pict, ....) :: Find
  fun Find.baseline_right(pict :: Pict, ....) :: Find
  fun Find.bottom(pict :: Pict, ....) :: Find
  fun Find.bottom_left(pict :: Pict, ....) :: Find
  fun Find.bottom_center(pict :: Pict, ....) :: Find
  fun Find.bottom_right(pict :: Pict, ....) :: Find
){

 Shorthands for @rhombus(Find) at all combinations of @rhombus(~horiz)
 and @rhombus(~vert) arguments in all orders. Each shorthand takes the
 same arguments ad @rhombus(Find), except for the @rhombus(~horiz) and
 @rhombus(~vert) arguments.

}
