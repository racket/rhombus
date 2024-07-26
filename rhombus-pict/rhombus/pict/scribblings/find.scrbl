#lang rhombus/scribble/manual

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
 two numbers representing an x-offset and y-offset, or to obtain one
 number representing a time offset in epochs.

}

@doc(
  fun Find(
    pict :: Pict,
    ~horiz: horiz :: HorizAlignment = #'center,
    ~vert: vert :: VertAlignment = #'center,
    ~dx: dx :: Real = 0,
    ~dy: dy :: Real = 0,
    ~dhoriz: dhoriz :: Real = 0,
    ~dvert: dvert :: Real = 0,
    ~dt: dt :: Int = 0
  ) :: Find
){

 Creates a @tech{finder} that locates a @rhombus(pict) within another
 @tech{pict}. The @rhombus(Find.in) method takes the pict that contains
 @rhombus(pict) and returns two numbers, the x-offset and y-offset, while
 the @rhombus(Find.start_in) method returns one number of the start of
 the found @rhombus(pict)'s @tech{time box} relative to a the start of
 the enclosing pict's time box as measured in epochs.

 The @rhombus(horiz) and @rhombus(vert) arguments determine the position
 within @rhombus(pict) that is found. For example, the combination
 @rhombus(#'right) and @rhombus(#'bottom) find the bottom-right corner of
 @rhombus(pict)'s bounding box within another pict.

 The @rhombus(dx) and @rhombus(dy) offsets are added to an x-offset and
 y-offset that would be returned otherwise. The @rhombus(dt) offset is
 add to a time-box difference that would be returned otherwise.

 The @rhombus(dhoriz) and @rhombus(dvert) arguments are multipled by the
 height and width, respectively, of the pict as located within another
 pict (which can be different than @rhombus(pict)'s immediate dimensions
 due to scaling). The multipled values are then added to an x-offset and
 y-offset that would be returned otherwise, in addition to @rhombus(dx)
 and @rhombus(dy).

}


@doc(
  fun Find.interpolate(from :: Find,
                       to :: Find,
                       ~bend: bender = bend.fast_middle)
    :: Find
){

 Creates a @tech{finder} that produces the same result as @rhombus(from)
 at the start of epoch 0 and before, the same result as @rhombus(to) at
 the end of epoch 0 and after, and that produces a combination of the two
 results for points in time within epoch 0. The combination is linear in
 the result of @rhombus(bender) applied to the relative time within the
 epoch's extent.

}


@doc(
  fun Find.abs(dx :: Real,
               dy :: Real,
               ~dt :: dt :: Int = 0) :: Find
){

 Creates a @tech{finder} that always returns @rhombus(dx) and
 @rhombus(dy) in space or @rhombus(dt) in time without needing to locate
 any particular component @tech{pict}.

}


@doc(
  fun Find.animate(xy_proc :: Function.of_arity(3),
                   ~time_box: t_proc :: Function.of_arity(1) = fun(p): 0)
    :: Find
){

 Creates a @tech{finder} that uses @rhombus(xy_proc) for
 @rhombus(Find.in) and @rhombus(t_proc) for @rhombus(Find.start_in).

}


@doc(
  method (finder :: Find).in(pict :: Pict) :: values(Real, Real)
  method (finder :: Find).in(pict :: Pict,
                             epoch :: Int, n :: RealIn(0, 1))
    :: values(Real, Real)
){

 Applies @rhombus(finder) to @rhombus(pict) to get an x-offset and
 y-offset. An exception is thrown is a needed component pict cannot be
 found in @rhombus(pict). Calling @rhombus(Find.in) with one argument
 is a shorthand for calling it with @rhombus(0) for the last two arguments.

 If @rhombus(pict) is an animated picture, then the search corresponds
 to finding within @rhombus(Pict.snapshot(pict, epoch, n)). When a finder for an
 animated pict is provdied to a function like @rhombus(pin), however,
 @rhombus(pin) will produce an animated pict where the finder is used
 separately for each snapshot generated from the combined animated pict.

}


@doc(
  method (finder :: Find).maybe_in(pict :: Pict)
    :: values(Real || False, Real || False)
  method (finder :: Find).maybe_in(pict :: Pict,
                                   epoch :: Int, n :: RealIn(0, 1))
    :: values(Real || False, Real || False)
){

 Like @rhombus(Find.in), but if a location cannot be found in
 @rhombus(pict), returns two @rhombus(#false)s instead of throwing an
 exception.

}


@doc(
  method (finder :: Find).start_in(pict :: Pict) :: Int
){

 Applies @rhombus(finder) to @rhombus(pict) to get a time-box offset. An
 exception is thrown is a needed component pict cannot be found in
 @rhombus(pict).

}


@doc(
  method (finder :: Find).delay(dt :: Int) :: Find
){

 Returns a @tech{finder} that is like @rhombus(finder), but where
 @rhombus(dt) is subtracted from the epoch hat is supplied to
 @rhombus(Find.in) or @rhombus(Find.maybe_in).

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
 same arguments as @rhombus(Find), except for the @rhombus(~horiz) and
 @rhombus(~vert) arguments.

}
