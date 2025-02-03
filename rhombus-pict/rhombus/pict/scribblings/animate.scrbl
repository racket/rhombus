#lang rhombus/scribble/manual

@(import:
    "pict_eval.rhm".pict_eval
    "plot.rhm".plot
    pict.bend
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "animate"){Pict Animators}

@doc(
  fun animate(
    snapshot
      :: Real.in(0, 1) -> StaticPict
      || (Real.in(0, 1), ~deps: List.of(Pict)) -> StaticPict
      || (Real.in(0, 1), ~config: maybe(Map)) -> StaticPict
      || (Real.in(0, 1), ~deps: List.of(Pict), ~config: maybe(Map)) -> StaticPict,
    ~dependencies: deps :: List.of(Pict) = [],
    ~config: config = maybe(Map) = #false,
    ~extent: extent :: NonnegReal = 0.5,
    ~bend: bender :: (Real.in(0, 1) -> Real.in(0, 1))
             = bend.fast_middle,
    ~sustain_edge: sustain_edge :: TimeOrder = #'after
  ) :: Pict
){

 Creates an @tech{animated pict}. The @rhombus(snapshot) function should
 accept a @rhombus(Real.in(), ~annot) and produce a
 @rhombus(StaticPict, ~annot). When @rhombus(Pict.snapshot) is called on
 the result @rhombus(p, ~var) from @rhombus(animate), the supplied
 @rhombus(snapshot) function is called. The pict produced by
 @rhombus(snapshot) is wrapped as a sole child of a new pict whose identity
 is the same as @rhombus(p, ~var); see also @secref("identity").

 The @rhombus(deps) list determines the
 @tech{replaceable dependencies} of @rhombus(animate)'s result,
 while a @rhombus(snapshot) result determines the @tech{findable children}. The
 @rhombus(snapshot) function is called with
 @rhombus(deps) as an optional @rhombus(~deps) argument, except that each
 pict in @rhombus(deps) is potentially adjusted through @rhombus(Pict.rebuild)
 or @rhombus(Pict.replace) before it is passed back to @rhombus(snapshot).
 The @rhombus(config) argument similarly provides a map that can be
 passed back to @rhombus(snapshot) as a @rhombus(~config) argument,
 possibly adjusted through @rhombus(Pict.rebuild) or
 @rhombus(Pict.configure).

 The resulting pict's @tech{duration} is 1, and the @tech{extent} of
 that duration is determined by the @rhombus(extent) argument. Before the
 pict's @tech{time box}, its drawing and @tech{bounding box} are the same
 as @rhombus(Pict.ghost(proc(0, & children))); after its time box, they are the
 same as @rhombus(Pict.ghost(proc(1, & children))).

 The @rhombus(bender) function adjusts the argument that is passed to
 @rhombus(proc). If @rhombus(bender) is the identity function, then the
 number passed to @rhombus(proc) ranges from @rhombus(0) to @rhombus(1)
 linearly with animation time. The default @rhombus(bend.fast_middle)
 keeps the argument in the range @rhombus(0) to @rhombus(1), but it
 causes the argument to @rhombus(proc) change more slowly near
 @rhombus(0) and @rhombus(1) and more quickly around @rhombus(0.5).

 If @rhombus(sustain_edge) is @rhombus(#'before), then
 @rhombus(Pict.sustain) for the resulting pict will extend its time box
 by adding an epoch to the start with the static representation
 @rhombus(Pict.ghost(proc(0))). If @rhombus(sustain_edge) is
 @rhombus(#'after), then @rhombus(Pict.sustain) instead adds to the end
 of the time box with the static representation
 @rhombus(Pict.ghost(proc(1))).

@examples(
  ~eval: pict_eval
  ~defn:
    fun bar(n):
      rectangle(~width: 30, ~height: 10 + n*10, ~fill: "lightblue")
  explain_anim(animate(bar))
  explain_anim(animate(bar).sustain())
  explain_anim(animate(bar, ~sustain_edge: #'before).sustain())
)

}

@doc(
  fun animate_map(
    combine
      :: (Int, Real.in(0, 1),
          ~deps: List.of(StaticPict)) -> StaticPict
      || (Int, Real.in(0, 1),
          ~deps: List.of(StaticPict),
          ~config: maybe(Map)) -> StaticPict,
    ~deps: deps :: List.of(Pict) = [],
    ~config: config :: maybe(Map) = #false,
    ~nonsustain_combine:
      nonsustain_combine
        :: maybe((Int, Real.in(0, 1),
                  ~deps: List.of(StaticPict)) -> StaticPict)
        || maybe((Int, Real.in(0, 1),
                  ~deps: List.of(StaticPict),
                  ~config: maybe(Map)) -> StaticPict)
        = #false,
    ~duration: duration_align :: DurationAlignment = #'sustain,
    ~epoch: epoch_align :: EpochAlignment = #'center
  ) :: Pict
){

 Creates a new pict by mapping a static-pict @rhombus(combine) function
 over snapshots (on demand) of the potentially animated picts
 @rhombus(deps). The @rhombus(combine) function receives an
 @rhombus(Int, ~annot) epoch, a @rhombus(Real.in(0, 1), ~annot) time
 within that epoch, and @rhombus(deps) back as a @rhombus(~deps)
 argument. If @rhombus(combine) accepts a @rhombus(~config) argument,
 then @rhombus(config) is passed back.

 The picts in @rhombus(deps) might be replaced via
 @rhombus(Pict.rebuild) or @rhombus(Pict.replace) on the result of
 @rhombus(animate_map), in which case the replacements are passed back to
 @rhombus(combine) as the @rhombus(~deps) argument. The optional
 @rhombus(config) similarly can be updated via @rhombus(Pict.rebuild) or
 @rhombus(Pict.configure) before it is passed back as the
 @rhombus(~config) argument to @rhombus(combine).

 If all picts in @rhombus(deps) satisfy @rhombus(StaticPict, ~annot) and
 @rhombus(nonsustain_combine) is @rhombus(#false), then the result also
 satisfies @rhombus(StaticPict, ~annot). In that case, @rhombus(combine)
 is called with @rhombus(0)s as its epoch and time arguments. Picts in
 @rhombus(deps) can be replaced with static or animated picts, and the
 rebuild result of @rhombus(animate_map) is accordingly static or
 animated.

 When @rhombus(deps) does not contain only
 @rhombus(StaticPict, ~annot)s, the @rhombus(deps) are made concurrent
 via @rhombus(concurrent), passing along @rhombus(duration_align) and
 @rhombus(epoch_align). The result pict's duration is the maximum of the
 durations of the @rhombus(deps).

 The @rhombus(combine) function is used for all epochs of the result
 from @rhombus(animate_map). If additional epochs are added to the result
 via @rhombus(Pict.sustain), those epochs also use @rhombus(combine). If
 @rhombus(nonsustain_combine) is a function, and if non-sustain epochs
 are added to the result of @rhombus(animate_map), then
 @rhombus(nonsustain_combine) is used for those epochs.

@examples(
  ~eval: pict_eval
  ~repl:
    def sq = square(~size: 32, ~fill: "lightblue")
    def sq_grow = animate(fun (n): sq.scale(1, 1 + n))
    explain_anim(
      animate_map(~deps: [sq_grow],
                  fun (i, n, ~deps: [sq :: Pict]):
                    stack(sq, text(str.f(sq.height,
                                         ~precision: 1))))
    )
)

}

@doc(
  property (pict :: Pict).duration :: Int
  method (pict :: Pict).epoch_extent(i :: Int) :: Real
){

 Properties for a @tech{pict}'s animation. See @secref("animated-pict")
 for more information.

@examples(
  ~eval: pict_eval
  ~repl:
    text("Hello").duration
    text("Hello").epoch_extent(0)
    switch(text("Hello"), text("World")).duration
  ~repl:
    def p = animate(fun (n): text("World").alpha(n))
    p.duration
    p.epoch_extent(0)
    switch(text("Hello"), p).epoch_extent(1)
)

}

@doc(
  method (pict :: Pict).snapshot(epoch :: Int = 0,
                                 n :: Real.in(0, 1) = 0,
                                 ~rebuild_prompt: rebuild_prompt = #true)
    :: StaticPict
){

 Converts an @tech{animated pict} to a @tech{static pict} for the
 animation at time @math{t} @math{=} @rhombus(epoch + n).

 If @rhombus(rebuild_prompt) is @rhombus(#false), then a rebuilding
 operation on the result pict via @rhombus(Pict.rebuild) is propagated to
 the original @rhombus(pict), and a snapshot of the rebuilt
 @rhombus(pict) is taken for the overall rebuild result. If
 @rhombus(rebuild_prompt) is a true value, then then a rebuilding
 operation rebuilds in the initial snapshot, instead of taking a snapshot
 of a rebuilt @rhombus(pict).

 The result snapshot's identity is the same as the identity of
 @rhombus(pict) if @rhombus(rebuild_prompt) is @rhombus(#true). If
 @rhombus(rebuild_prompt) is @rhombus(#false), the result identity is
 fresh and it depends on @rhombus(pict), but the snapshot is also
 findable as @rhombus(pict). See also @secref("identity").

@examples(
  ~eval: pict_eval
  ~repl:
    def p = animate(fun (n): text("World").alpha(1-n))
    p.snapshot()
    p.snapshot(0, 0.5)
    p.snapshot(0, 0.75)
)

}

@doc(
  method (pict :: Pict).time_pad(
    ~all: all :: Int = 0,
    ~before: before :: Int = all,
    ~after: after :: Int = all
  ) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with its
 @tech{time box} adjusted.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen")
    def q = p.time_pad(~after: 2)
    p.duration
    q.duration
    rectangle(~around: q.snapshot(0, 0.0))
    rectangle(~around: q.snapshot(2, 0.0))
)

}

@doc(
  method (pict :: Pict).sustain(n :: Int = 1) :: Pict
){

 Similar to @rhombus(Pict.time_pad) with @rhombus(~after), but
 @tech{sustains} instead of merely padding.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen")
    def q = p.sustain(2)
    q.duration
    rectangle(~around: q.snapshot(2, 0.0))
)

}

@doc(
  method (pict :: Pict).nonsustaining() :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but where a
 @tech{sustain} operation is treated the same as padding via
 @rhombus(Pict.time_pad).

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen").nonsustaining()
    def q = p.sustain(2)
    q.duration
    rectangle(~around: q.snapshot(2, 0.0))
)

}

@doc(
  method (pict :: Pict).time_clip(
    ~keep: keep :: maybe(TimeOrder) = #false,
    ~nonsustaining: nonsustaining = keep != #'after
  ) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 confined to its time box in the sense that it is represented by
 @rhombus(nothing) outside of its time box. If @rhombus(keep) is
 @rhombus(#'before) or @rhombus(#'after), then the pict is not clipped in
 that time direction. The resulting pict is nonsustaining (in the sense
 of @rhombus(Pict.nonsustaining)) if @rhombus(nonsustaining) is true.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen")
    rectangle(~around: p.snapshot(0, 0.0))
    rectangle(~around: p.snapshot(2, 0.0))
    rectangle(~around: p.time_clip().snapshot(2, 0.0))
    rectangle(~around: p.time_clip().snapshot(2, 0.0)) is_a NothingPict
)

}

@doc(
  method (pict :: Pict).instantaneous() :: Pict
){

 Creates a @tech{pict} of duration @rhombus(1) that draws a snapshot of
 @rhombus(pict) at the start of its @tech{time box} and nothing before or
 after the start of the time box.

 The result is the same as

@rhombusblock(
  Pict.time_clip(animate(fun (n): if n .= 0 | pict.snapshot() | nothing,
                         ~extent: 0))
)

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen")
    p.snapshot(0, 0.1)
    p.instantaneous().snapshot(0, 0.1)
    p.instantaneous().snapshot(0, 0.1) is_a NothingPict
)

}


@doc(
  method (pict :: Pict).time_insert(epoch :: Int,
                                    n_epochs :: NonnegInt) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but potentially with
 a longer @tech{duration}. The result is @rhombus(pict) itself if
 @rhombus(epoch) is outside of @rhombus(pict)'s @tech{time box} or if
 @rhombus(n_epochs) is @rhombus(0).

 When @rhombus(epoch) is within @rhombus(pict)'s time box---that is,
 when it is between @rhombus(0) (inclusive) and the duration of
 @rhombus(pict) (exclusive), then the duration of the resulting pict is
 larger by @rhombus(n_epochs). The duration is extended by adding epochs
 that render as @rhombus(Pict.snapshot(pict, epoch, 0)) before the epoch
 at index @rhombus(epoch).

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen")
    def q = p.time_insert(0, 3)
    rectangle(~around: q.snapshot(3, 0.0))
    rectangle(~around: q.snapshot(4, 0.0))
)

}


@doc(
  method (pict :: Pict).epoch_set_extent(i :: Int,
                                         extent :: NonnegReal) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but with the
 @tech{extent} of one of its @tech{epochs} adjusted.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = circle(~size: 20, ~fill: "lightgreen")
    p.epoch_extent(0)
    def q = q.epoch_set_extent(0, 0.5)
    q.epoch_extent(0)
)

}

@doc(
  fun bend.fast_start(n :: Real.in(0, 1)) :: Real.in(0, 1)
  fun bend.fast_middle(n :: Real.in(0, 1)) :: Real.in(0, 1)
  fun bend.fast_edges(n :: Real.in(0, 1)) :: Real.in(0, 1)
  fun bend.fast_end(n :: Real.in(0, 1)) :: Real.in(0, 1)
){

 Functions that are useful as a @rhombus(~bend) argument to
 @rhombus(animate) to map a linear animation effect to a non-linear one.

@tabular(~sep: @hspace(2),
         ~column_properties: [#'center],
         [[hspace(1), rhombus(bend.fast_start), rhombus(bend.fast_middle),  rhombus(bend.fast_edges),  rhombus(bend.fast_end)],
          [hspace(1), "", "", "", ""],
          ["", plot(bend.fast_start), plot(bend.fast_middle),  plot(bend.fast_edges),  plot(bend.fast_end)]])

}


@doc(
  fun explain_anim(
    pict :: Pict,
    ~start: start :: Int = 0,
    ~end: end :: Int = math.max(p.duration, start),
    ~steps: steps :: PosInt = 4,
    ~steps_before: steps_before :: NonnegInt = 0,
    ~steps_after: steps_after :: NonnegInt = 0,
    ~show_timebox: show_timebox = #true,
    ~pad_before: pad_before :: NonnegInt = 0,
    ~pad_after: pad_after :: NonnegInt = 0
  ) :: Pict
){

 Returns a pict that shows a timeline with snapshots of @rhombus(pict)'s
 drawing at different points. The timesline shows @tech{epochs}
 @rhombus(start) through @rhombus(end), inclusive. If @rhombus(steps) is
 greater than @rhombus(1), then @rhombus(steps-1) intermediate points are
 shown for each complete epoch. Use @rhombus(steps_before) to add extra
 steps before @rhombus(start) and @rhombus(steps_after) to add extra
 steps after @rhombus(end).

 If @rhombus(show_timebox) is a true value, then a box is drawn around
 points on the timeline that are within @rhombus(pict)'s @tech{time box}.

 If @rhombus(pad_before) or @rhombus(pad_after) are not @rhombus(0),
 then space is added to the left or right of the timeline, respectively,
 corresponding to @rhombus(pad_before) or @rhombus(pad_after) epochs.
 This padding can be useful for aligning timelines that have different
 starting or ending epochs.

@examples(
  ~eval: pict_eval
  explain_anim(animate(fun (n): circle(~fill: "blue").alpha(1-n)),
               ~steps: 5)
)

}
