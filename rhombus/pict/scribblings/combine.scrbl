#lang scribble/rhombus/manual

@(import:
    "pict_eval.rhm".pict_eval
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "combine"){Pict Combiners}

@doc(
  fun beside(
    ~sep: sep :: Real = 0,
    ~vert: vert_align :: VertAlignment = #'center,
    ~order: order :: OverlayOrder = #'front,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that combines the given @rhombus(pict)s horizontally
 with the first @rhombus(pict) as leftmost.

 If a pict extends horizontally outside its bounding box, then the
 front-ot-back order of picts can matter for the combined image. The
 @rhombus(order) argument determines the order of each pict added to the
 right of the combined image.

 The picts are first made concurrent via @rhombus(concurrent), passing
 along @rhombus(duration_align) and @rhombus(epoch_align).

 If no @rhombus(pict)s are provided, the result is @rhombus(nothing).

@examples(
  ~eval: pict_eval
  beside(square(~size: 32), circle(~size: 16))
  beside(~vert: #'top, ~sep: 12, square(~size: 32), circle(~size: 16))
)

}

@doc(
  fun stack(
    ~sep: sep :: Real = 0,
    ~horiz: horiz_alig :: HorizAlignment = #'center,
    ~order: order :: OverlayOrder = #'front,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that combines the given @rhombus(pict)s vertically with
 the first @rhombus(pict) as topmost.

 If a pict extends vertically outside its bounding box, then the
 front-ot-back order of picts can matter for the combined image. The
 @rhombus(order) argument determines the order of each pict added to the
 bottom of the combined image.

 The picts are first made concurrent via @rhombus(concurrent), passing
 along @rhombus(duration_align) and @rhombus(epoch_align).

 If no @rhombus(pict)s are provided, the result is @rhombus(nothing).

@examples(
  ~eval: pict_eval
  stack(square(~size: 32), circle(~size: 16))
  stack(~horiz: #'right, ~sep: 12, square(~size: 32), circle(~size: 16))
)

}

@doc(
  fun overlay(
    ~horiz: horiz_align :: HorizAlignment = #'center,
    ~vert: vert_align :: VertAlignment = #'center,
    ~order: order :: OverlayOrder = #'front,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that combines the given @rhombus(pict)s by overlaying.
 The @rhombus(order) argument determines whether later @rhombus(pict)s
 are placed in front of earlier @rhombus(pict)s or behind them.

 The picts are first made concurrent via @rhombus(concurrent), passing
 along @rhombus(duration_align) and @rhombus(epoch_align).

 If no @rhombus(pict)s are provided, the result is @rhombus(nothing).

@examples(
  ~eval: pict_eval
  overlay(square(~size: 32, ~fill: "lightblue"),
          circle(~size: 16, ~fill: "lightgreen"))
  overlay(~horiz: #'right, ~vert: #'bottom, ~order: #'back,
          square(~size: 32),
          circle(~size: 16, ~line: "red"))
)


}

@doc(
  fun pin(
    pict :: Pict,
    ~on: on_pict :: Pict,
    ~at: finder :: Find,
    ~find: find_mode :: FindMode = #'always,
    ~pinhole: pinhole_finder :: Find = Find.left_top(q),
    ~order: order :: OverlayOrder = #'front,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~time: time_align :: TimeAlignment = #'start,
    ~epoch: epoch_align :: EpochAlignment = #'center
  ) :: Pict
){

 Returns a @tech{pict} that draws @rhombus(pict) in front of or behind
 @rhombus(on_pict) at the location in @rhombus(on_pict) determined by
 @rhombus(finder), where the location determined by @rhombus(pinhole_finder)
 in @rhombus(pict) is matched with that location. The resulting pict's
 bounding box is the same as @rhombus(on_pict)'s.

 By default, the pinned @rhombus(pict) is concurrent to the target
 @rhombus(on_pict). The picts are first made concurrent via
 @rhombus(concurrent), passing along @rhombus(duration_align) and
 @rhombus(epoch_align). This mode is used unless
 @rhombus(time_align) is @rhombus(#'insert).

 The @rhombus(time_align) argument determines a @tech{time box} offset that is
 applied to @rhombus(pict), @rhombus(finder), and @rhombus(pinhole_finder)
 before combining it with @rhombus(on_pict).
 If @rhombus(time_align) is @rhombus(#'insert) or @rhombus(~sync),
 then @rhombus(finder) is used both to find
 a graphical offset and a @tech{time box} offset. If @rhombus(time_align)
 is @rhombus(insert), then at the time box offset within
 @rhombus(on_pict), extra epochs are inserted that correspond to a
 snapshot of @rhombus(on_pict) at the time box offset, and the number of
 inserted epochs is the @tech{duration} of @rhombus(pict). When @rhombus(time_align)
 When @rhombus(time_align) is an integer, @rhombus(#'start), or @rhombus(#'end), then
 @rhombus(finder) is @emph{not} used to find a time offset, and instead
 @rhombus(time_align), @rhombus(0), or @rhombus(Pict.duration(on_pict)) is used,
 respectively.

 If @rhombus(find_mode) is @rhombus(#'always), then if @rhombus(finder)
 fails to find a position at any time for @rhombus(on_pict), then an
 exception is thrown. If @rhombus(find_mode) is @rhombus(#'maybe), then
 when @rhombus(finder) fails to find a position, @rhombus(pict) is not
 pinned.

@examples(
  ~eval: pict_eval
  def circ = circle(~size: 16, ~fill: "lightgreen")
  pin(~on: overlay(square(~size: 32, ~fill: "lightblue"), circ),
      ~at: Find.right(circ),
      line(~dx: 10))
)

}

@doc(
  fun connect(
    ~on: on_pict :: Pict,
    from :: Find,
    to :: Find,
    ~find: find_mode :: FindMode = #'always,
    ~style: style :: ConnectStyle = #'line,
    ~line: color :: ConnectMode = #'inherit,
    ~line_width: width :: LineWidth = #'inherit,
    ~order: order :: OverlayOrder = #'front,
    ~arrow_size: arrow_size :: Real = 16,
    ~arrow_solid: solid = #true,
    ~arrow_hidden: hidden = #false,
    ~start_angle: start_angle :: maybe(Real) = #false,
    ~start_pull: start_pull :: maybe(Real) = #false,
    ~end_angle: end_angle :: maybe(Real) = #false,
    ~end_pull: end_pull :: maybe(Real) = #false,
    ~label: label :: maybe(Pict) = #false,
    ~label_dx: label_dx :: Real = 0,
    ~label_dy: label_dy :: Real = 0,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~epoch: epoch_align :: EpochAlignment = #'center
  ) :: Pict
){

 Returns a @tech{pict} like @rhombus(on_pict), but with a line added to
 connect @rhombus(from) to @rhombus(to).

 If @rhombus(find_mode) is @rhombus(#'always), then if @rhombus(form) or
 @rhombus(to) fails to find a position at any time for @rhombus(on_pict),
 then an exception is thrown. If @rhombus(find_mode) is
 @rhombus(#'maybe), then when @rhombus(from) or @rhombus(to) fails to
 find a position, a line is not added.

@examples(
  ~eval: pict_eval
  def circ = circle(~size: 16, ~fill: "lightgreen")
  def sq = square(~size: 32, ~fill: "lightblue")
  connect(~on: beside(~sep: 32, sq, circ),
          Find.right(sq),
          Find.left(circ),
          ~style: #'arrow,
          ~line: "red",
          ~arrow_size: 8)
)


}

@doc(
  fun table(
    rows :: List.of(List),
    ~horiz: horiz :: HorizAlignment || List.of(HorizAlignment) = #'left,
    ~vert: vert :: VertAlignment || List.of(VertAlignment) = #'topline,
    ~hsep: hsep :: Real || List.of(Real) = 32,
    ~vsep: vsep :: Real || List.of(Real) = 1,
    ~pad: pad :: matching((_ :: Real)
                            || [_ :: Real, _ :: Real]
                            || [_ :: Real, _ :: Real, _ :: Real, _ :: Real])
            = 0,
    ~line: line_c :: maybe(ColorMode) = #false,
    ~line_width: line_width :: LineWidth = #'inherit,
    ~hline: hline :: maybe(ColorMode) = line_c,
    ~hline_width: hline_width :: LineWidth = line_width,
    ~vline: vline :: maybe(ColorMode) = line_c,
    ~vline_width: vline_width :: LineWidth = line_width
  ) :: Pict
){

 Creates a table @tech{pict}. For @rhombus(horiz), @rhombus(vert),
 @rhombus(vsep), and @rhombus(hsep), a value or final list element is
 repeated as meany times as needed to cover all rows, columns, or
 positions between them, and extra list elements are ignored.

@examples(
  ~eval: pict_eval
  def circ = circle(~size: 16, ~fill: "lightgreen")
  def sq = square(~size: 32, ~fill: "lightblue")
  table([[blank(),              text("Square"), text("Circle")],
         [blank(),              sq,             circ],
         [text("rolls"),        blank(),        text("✔")],
         [text("easy to cut"),  text("✔"),      blank()]],
        ~horiz: [#'left, #'center],
        ~vert: #'top,
        ~vline: "black",
        ~pad: 5)
)

}


@doc(
  fun switch(
    ~splice: splice :: maybe(TimeOrder) = #false,
    ~join: join :: SequentialJoin = if splice | #'splice | #'step,
    pict :: Pict, ...
  ) :: Pict
){

 Creates a pict that has the total duration of the given
 @rhombus(pict)s, where the resulting pict switches from one pict at the
 end of its time box to the next. The result pict's rendering before its
 timebox is the same as the first @rhombus(pict), and its rendering after
 is the same as the last @rhombus(pict).

 If no @rhombus(pict)s are provided, the result is @rhombus(nothing).

}

@doc(
  fun concurrent(
    ~duration: duration_align :: DurationAlignment = #'pad,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    pict :: Pict, ...
  ) :: List.of(Pict)
){

 Returns a list of @tech{picts} like the given @rhombus(pict)s, except that time
 boxes and epochs of each are extended to match, including the same
 extent for each epoch in the time box.

 If @rhombus(duration_align) is @rhombus(#'pad), the time boxes are
 extended as needed in the ``after'' direction using @rhombus(Pict.pad).
 If @rhombus(duration_align) is @rhombus(#'sustain), then
 @rhombus(Pict.sustain) is used. Note that the default for
 @rhombus(duration_align) is @rhombus(#'pad), but when
 @rhombus(concurrent) is called by functions like @rhombus(beside), the
 defult is @rhombus(#'sustain).

 The @rhombus(epoch_align) argument determines how animations are
 positioned within an extent when extents are made larger to synchronize
 with concurrent, non-@rhombus(0) extents.

 Any @rhombus(nothing) among the @rhombus(pict)s is preserved in the
 output list, but it does not otherwise particiapte in making the other
 @rhombus(pict)s concurrent.

}


@doc(
  fun sequential(
    ~join: mode :: SequentialJoin = #'step,
    ~duration: duration_align :: DurationAlignment = #'pad,
    ~concurrent: to_concurrent = #true,
    pict :: Pict, ...
  ) :: List.of(AnimPict)
){

 Returns a list of @tech{picts} like the given @rhombus(pict)s, except
 the time box of each is padded in the ``before'' direction to
 sequentialize the picts.

 If @rhombus(to_concurrent) is true, then after the picts are
 sequentialized, they are passed to @rhombus(#'concurrent). The
 @rhombus(duration_align) argument is passed along in that case.

 Any @rhombus(nothing) among the @rhombus(pict)s is preserved in the
 output list, but it does not otherwise particiapte in making the other
 @rhombus(pict)s sequential.

}

@doc(
  fun animate_map(
    picts :~ List.of(Pict),
    ~combine: combine :: Function.of_arity(2),
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~epoch: epoch_align :: EpochAlignment = #'center,
    ~non_sustain_combine: non_sustain_combine :: Function.of_arity(3)
                            = combine
  ) :: Pict
){

 Constructs a @tech{pict} by lifting an operation on @tech{static picts}
 to one on @tech{animated picts}. The @rhombus(combine) function is
 called as needed on a list of static picts corresponding to the input
 @rhombus(pict)s, and it should return a static pict. In addition to a
 list of list of static picts, @rhombus(combine) receives an integer epoch
 offset and a real number in 0 to 1 for a relative time within the epoch.

 The picts are first made concurrent via @rhombus(concurrent), passing
 along @rhombus(duration_align) and @rhombus(epoch_align).

}

@doc(
  fun beside.top(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
  fun beside.topline(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
  fun beside.center(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
  fun beside.baseline(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
  fun beside.bottom(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
){

 Shorthands for @rhombus(beside)  with a @rhombus(~vert) argument.

}

@doc(
  fun stack.center(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
  fun stack.left(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
  fun stack.right(~sep: sep :: Real = 0, pict :: Pict, ...) :: Pict
){

 Shorthands for @rhombus(stack)  with a @rhombus(~horiz) argument.

}

@doc(
  fun overlay.center(pict :: Pict, ...) :: Pict
  fun overlay.left(pict :: Pict, ...) :: Pict
  fun overlay.right(pict :: Pict, ...) :: Pict
  fun overlay.top(pict :: Pict, ...) :: Pict
  fun overlay.topline(pict :: Pict, ...) :: Pict
  fun overlay.baseline(pict :: Pict, ...) :: Pict
  fun overlay.bottom(pict :: Pict, ...) :: Pict
  fun overlay.left_top(pict :: Pict, ...) :: Pict
  fun overlay.left_topline(pict :: Pict, ...) :: Pict
  fun overlay.left_center(pict :: Pict, ...) :: Pict
  fun overlay.left_baseline(pict :: Pict, ...) :: Pict
  fun overlay.left_bottom(pict :: Pict, ...) :: Pict
  fun overlay.center_top(pict :: Pict, ...) :: Pict
  fun overlay.center_topline(pict :: Pict, ...) :: Pict
  fun overlay.center_center(pict :: Pict, ...) :: Pict
  fun overlay.center_baseline(pict :: Pict, ...) :: Pict
  fun overlay.center_bottom(pict :: Pict, ...) :: Pict
  fun overlay.right_top(pict :: Pict, ...) :: Pict
  fun overlay.right_topline(pict :: Pict, ...) :: Pict
  fun overlay.right_center(pict :: Pict, ...) :: Pict
  fun overlay.right_baseline(pict :: Pict, ...) :: Pict
  fun overlay.right_bottom(pict :: Pict, ...) :: Pict
  fun overlay.top_left(pict :: Pict, ...) :: Pict
  fun overlay.top_center(pict :: Pict, ...) :: Pict
  fun overlay.top_right(pict :: Pict, ...) :: Pict
  fun overlay.topline_left(pict :: Pict, ...) :: Pict
  fun overlay.topline_center(pict :: Pict, ...) :: Pict
  fun overlay.center_left(pict :: Pict, ...) :: Pict
  fun overlay.center_right(pict :: Pict, ...) :: Pict
  fun overlay.baseline_left(pict :: Pict, ...) :: Pict
  fun overlay.baseline_center(pict :: Pict, ...) :: Pict
  fun overlay.baseline_right(pict :: Pict, ...) :: Pict
  fun overlay.bottom(pict :: Pict, ...) :: Pict
  fun overlay.bottom_left(pict :: Pict, ...) :: Pict
  fun overlay.bottom_center(pict :: Pict, ...) :: Pict
  fun overlay.bottom_right(pict :: Pict, ...) :: Pict
){

 Shorthands for @rhombus(overlay) at all combinations of @rhombus(~horiz)
 and @rhombus(~vert) arguments in all orders.

}

@doc(
  enum HorizAlignment:
    left
    center
    right
){

 Options for horizontal alignment.

}

@doc(
  enum VertAlignment:
    top
    topline
    center
    baseline
    bottom
){

 Options for vertical alignment.

}

@doc(
  enum DurationAlignment:
    pad
    sustain
){

 Options for duration alignment.

}

@doc(
  enum TimeAlignment:
    ~is_a Int
    start
    insert
    sync
    end
){

 Options for time alignment and insertion when pinning.

}

@doc(
  enum EpochAlignment:
    early
    center
    stretch
    late
){

 Options for epoch alignment.

}

@doc(
  enum SequentialJoin:
    step
    splice
){

 Options for sequential joining.

}


@doc(
  enum OverlayOrder:
    front
    back
){

 Options for overlaying.

}

@doc(
  enum FindMode:
    always
    maybe
){

 Options for handling finder failure in function such as @rhombus(pin)
 and @rhombus(connect).

}


@doc(
  enum ConnectStyle:
    line
    arrow
    arrows
){

 Options for a @rhombus(connect) style.

}
