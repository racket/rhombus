#lang rhombus/scribble/manual

@(import:
    "pict_eval.rhm".pict_eval    
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "pict"){Pict Objects}

@doc(
  annot.macro 'Pict'
  annot.macro 'StaticPict':
    ~method_fallback: Pict
){

 The @rhombus(Pict) annotation is satisfied by any @tech{pict} value.
 The @rhombus(StaticPict) annotation is statisfied only by a pict that is
 specifically a @tech{static pict}.

@examples(
  ~eval: pict_eval
  ~repl:
    rectangle() is_a Pict
    rectangle() is_a StaticPict
    animate(fun (n): rectangle()) is_a StaticPict
)

}

@doc(
  def nothing :: NothingPict
  annot.macro 'NothingPict':
    ~method_fallback: Pict
){

 The @rhombus(nothing) pict is a special @tech{static pict} that is
 ignored (i.e., as if it is not supplied at all) by pict primitive
 constructors. The @rhombus(NothingPict, ~annot) annotation is satisfied
 by only @rhombus(nothing).

@examples(
  ~eval: pict_eval
  ~repl:
    beside(stack(~sep: 5, text("Hello"), nothing, nothing, text("World")),
           stack(~sep: 5, text("Hello"), text("World")))
)

}

@doc(
  property (pict :: Pict).width :: Real
  property (pict :: Pict).height :: Real
  property (pict :: Pict).ascent :: Real
  property (pict :: Pict).descent :: Real
){

 Properties for a @tech{pict}'s @tech{bounding box}. See
 @secref("static-pict") for an explanation of bounding boxes.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = stack(text("Hello"), text("Pict"), text("World"))
    explain_bbox(p)
    p.width
    p.height
    p.ascent
    p.descent
    explain_bbox(p.pad(-5)).pad(8)
)

}

@doc(
  method (pict :: Pict).pad(
    around :: Real = 0,
    ~horiz: horiz :: Real = around,
    ~vert: vert :: Real = around,
    ~left: left :: Real = horiz,
    ~top: top :: Real = vert,
    ~right: right :: Real = horiz,
    ~bottom: bottom :: Real = vert
  ) :: Pict
  method (pict :: Pict).drop_baseline(amt :: Real) :: Pict
  method (pict :: Pict).drop_topline(amt :: Real) :: Pict
  method (pict :: Pict).translate(dx :: Real, dy :: Real) :: Pict
){

 Returns a @tech{pict} that draws the same as @rhombus(pict), but with
 its @tech{bounding box} adjusted. The @rhombus(Pict.pad) function
 conceptually adds padding around the outside of a bounding box, shifting
 it away from the inside of the pict's drawing; negative padding shifts a
 bounding box toward the inside. The @rhombus(Pict.drop_baseline) and
 @rhombus(Pict.drop_topline) functions adjust the bounding box's decent
 (subtracting @rhombus(amt)) and ascent (adding @rhombus(amt)),
 respectively. The @rhombus(Pict.translate) function shifts drawing
 relative to its bounding box, which keeps the same width, height,
 ascent, and descent.

@examples(
  ~eval: pict_eval
  ~repl:
    explain_bbox(text("Hello"))
    explain_bbox(text("Hello").pad(5))
    explain_bbox(text("Hello").pad(~left: 5, ~right: 10))
    explain_bbox(beside(~vert: #'topline,
                        text("Hello").drop_topline(-3),
                        text("World")))
    explain_bbox(text("Hello").translate(5, -5))
)

}

@doc(
  method (pict :: Pict).scale(n :: Real) :: Pict
  method (pict :: Pict).scale(horiz :: Real, vert :: Real) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but its drawing and
 bounding box is scaled.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello").colorize("firebrick")
    p
    p.scale(2)
)

}

@doc(
  method (pict :: Pict).rotate(radians :: Real) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but its drawing is
 rotated, and its bounding box is extended as needed to enclose the
 rotated bounding box.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello").colorize("firebrick")
    p.rotate(math.pi/4)
)

}

@doc(
  method (pict :: Pict).shear(x_factor :: Real,
                              y_factor :: Real) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but its drawing is
 sheared by progressively adjusting the x-position of drawing so that it
 is shifted by @rhombus(x_factor) of the height by the bottom of the
 bounding box, and by adjusting the y-positio of drawing to shift it by
 @rhombus(y_factor) of the width by the right edge of the bounding box.
 The bounding box is inflated to contain the result. The result pict's
 ascent and descent are the same as @rhombus(pict)'s.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello").colorize("firebrick")
    p.shear(0.5, 0)
    p.shear(0, 0.5)
)

}

@doc(
  method (pict :: Pict).alpha(n :: Real.in(0, 1)) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 changed by multiplying @rhombus(n) to an inherited alpha adjustment.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello").colorize("firebrick")
    p
    p.alpha(0.5)
    p.alpha(0.25)
)

}

@doc(
  method (pict :: Pict).hflip() :: Pict
  method (pict :: Pict).vflip() :: Pict
){

 Flips a @tech{pict} horizontally or vertically.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello").colorize("firebrick")
    p.hflip()
    p.vflip()
)

}

@doc(
  method (pict :: Pict).clip() :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 confined to its bounding box.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello").colorize("firebrick").pad(-5).scale(2)
    rectangle(~around: p).pad(10)
    rectangle(~around: p.clip()).pad(10)
)

}

@doc(
  method (pict :: Pict).colorize(c :: Color || String) :: Pict
  method (pict :: Pict).line_width(w :: NonnegReal) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but wherever it uses
 @rhombus(#'inherit) for a color or line width, the given color or line
 width is used, and the resulting pict no longer uses @rhombus(#'inherit)
 or colors or line widths.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = square(~size: 20)
    p
    p.colorize("firebrick").line_width(2)
  ~repl:
    def p = square(~size: 20,
                   ~line_width: 2,
                   ~line: #'inherit,
                   ~fill: "lightblue")
    p
    p.colorize("firebrick").line_width(0)
)

}


@doc(
  property (pict :: Pict).children :: List.of(Pict)
){

 A list of component @tech{picts} that were combined to construct
 @rhombus(pict). Those picts can be located within @rhombus(pict) using a
 @tech{finder}.

@examples(
  ~eval: pict_eval
  ~repl:
    def h = text("Hello").colorize("firebrick")
    def w = text("World").colorize("firebrick")
    h.children
    stack(h, w)
    stack(h, w).children
    Find.top_left(w).in(stack(h, w))
)

}

@doc(
  method (pict :: Pict).launder() :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with a
 fresh identity and hiding the identity of any component inside
 @rhombus(pict) from a @tech{finder}, traversal via
 @rhombus(Pict.rebuild), or the result of the @rhombus(Pict.children)
 property.

@examples(
  ~eval: pict_eval
  ~repl:
    def h = text("Hello")
    def w = text("World")
    def p = stack(h, w)
    Find.top_left(w).in(p)
    def q = p.launder()
    ~error:
      Find.top_left(w).in(q)
)

}

@doc(
  method (pict :: Pict).ghost(do_ghost = #true) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), including the
 same @tech{bounding box} and @tech{time box}, but whose drawing is empty
 if @rhombus(do_ghost) is true. If @rhombus(do_ghost) is @rhombus(#false),
 then @rhombus(pict) itself is returned.

 The @rhombus(do_ghost) argument is intended to help avoid @rhombus(if)
 wrappers, enabling @rhombus(pict.ghost(#,(@rhombus(test, ~var)))) instead of
 @rhombus(if #,(@rhombus(test, ~var)) | pict.ghost() | pict), where the
 former works even without having to bind an intermediate variable if
 @rhombus(pict) is replaced with a more complex expression.

@examples(
  ~eval: pict_eval
  ~repl:
    def p = text("Hello")
    rectangle(~around: p)
    rectangle(~around: p.ghost())
    rectangle(~around: p.ghost(#false))
)

}

@doc(
  method (pict :: Pict).refocus(subpict :: Pict) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with a
 shifted @tech{bounding box} to match the binding box of @rhombus(subpict) within
 @rhombus(pict).

@examples(
  ~eval: pict_eval
  ~repl:
    def h = text("Hello")
    def p = beside(circle(~size: 20, ~fill: "lightgreen"),
                   stack(h, text("World")),
                   square(~size: 20, ~fill: "lightblue"))
    explain_bbox(p).pad(20)
    explain_bbox(p.refocus(h)).pad(20)
)

}

@doc(
  method (pict :: Pict).rebuild(
    ~pre: pre_adjust :: Function.of_arity(1),
    ~post: post_adjust :: Function.of_arity(1),
    ~configure: config_adjust :: Function.of_arity(1)
                  = fun(config): config
  ) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction with @rhombus(pre_adjust) and
 @rhombus(post_adjust) applied to each component pict, and with
 @rhombus(config_adjust) applied to each configuration map of a pict
 created by @rhombus(rebuildable).

 To support @rhombus(Pict.rebuild), the representation of a pict
 effectively records all primitive operations used to construct the pict.
 This recording is limited to @rhombus(Pict, ~annot) and @rhombus(Find, ~annot) objects.
 If, for example, you use @rhombus(Find.in) to obtain a number and then
 construct a pict using that number, the number itself cannot record its
 derivation from the picts used with @rhombus(Find.in). In such cases,
 use @rhombus(rebuildable) or the @rhombus(~children) argument of
 @rhombus(animate) to establish a connection between the input picts and
 the result.

 When traversing the children of @rhombus(pict) to rebuild it, the
 @rhombus(pre_adjust) function is applied to each pict before its own
 children. When @rhombus(pre_adjust) returns a value other than the pict
 that it is given, rebuilding does not recur to its children, and the
 result of @rhombus(pre_adjust) is used immediately as the rebuilt
 replacement for the given pict. When rebuilding does recur, when a
 pict's descendants are unchanged and when @rhombus(config_adjust) (if
 applicable) returns a configuration unchanged, then the original
 construction of the pict is kept, preserving its identity (while picts
 will have fresh identities when they are rebuilt due to a changed child
 or configuration). Finally, @rhombus(post_adjust) is applied to either
 the rebuilt pict or so-far-preserved original pict to obtain the rebuilt
 replacement for the original pict. The replacement of a given pict is
 cached, so @rhombus(pre_adjust) and @rhombus(post_adjust) are each
 applied at most once to a pict within a call to @rhombus(Pict.rebuild).

@examples(
  ~eval: pict_eval
  def s = square(~size: 20, ~fill: "blue")
  def p = beside(~sep: 10, s, Pict.launder(s))
  p
  p.rebuild(~pre: fun (q): if q == s | s.scale(2) | q)
  p.rebuild(~pre: fun (q): if q == s | s.scale(2) | q,
            ~post: fun (q :~ Pict):
                     rectangle(~around: q.pad(2), ~line: "red"))
)

}

@doc(
  method (pict :: Pict).replace(orig :: Pict,
                                replacement :: Pict) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction to replace each use of @rhombus(orig)
 with @rhombus(replacement).

 This operation is equivalent to a use of @rhombus(Pict.rebuild):

@rhombusblock(
  pict.rebuild(~pre: fun (p):
                       if p == orig
                       | replacement
                       | p)
)

@examples(
  ~eval: pict_eval
  def s = square(~size: 20, ~fill: "blue")
  def c = circle(~size: 15, ~fill: "red")
  def p = beside(~sep: 10, s, s)
  p
  p.replace(s, c)
)

}

@doc(
  method (pict :: Pict).configure(key :: Any, vall :: Any) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction to replace any configuration map entry for
 @rhombus(key) with @rhombus(val).

 This operation is equivalent to a use of @rhombus(Pict.rebuild):

@rhombusblock(
  pict.rebuild(~configure:
                 fun (config :: Map):
                   if config.has_key(key)
                   | config ++ { key: val }
                   | config)
)
}


@doc(
  method (pict :: Pict).freeze(~scale: scale :: Real = 2.0) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 rendered at the time of the @rhombus(Pict.freeze) call to a bitmap, and
 then the new pict draws by drawing the bitmap. The @rhombus(scale)
 argument determines the scale factor of the bitmap (i.e., the number of
 pixels per drawing unit).

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
  method (pict :: Pict).snapshot() :: StaticPict
  method (pict :: Pict).snapshot(epoch :: Int, n :: Real.in(0, 1))
    :: StaticPict
){

 Converts an @tech{animated pict} to a @tech{static pict} for the
 animation at time @math{t} @math{=} @rhombus(epoch + n). The 0-argument
 variant is a shorthand for providing @rhombus(0) and @rhombus(0).

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
  method (pict :: Pict).metadata() :: Map
  method (pict :: Pict).set_metadata(metadata :: Map) :: Pict
){

 Gets metadata registered for an immediate pict, or returns a
 @tech{pict} that is like @rhombus(pict) but with the given metadata.

 Unlike epoch-specific metadata added with
 @rhombus(Pict.epoch_set_metadata), metadata added by
 @rhombus(Pict.set_metadata) is not propagated to new picts that are derived
 from the returned pict. The @rhombus(Pict.children) property of a pict
 can be used to search the metadata of its component picts.

}

@doc(
  method (pict :: Pict).epoch_metadata(i :: Int) :: Map
  method (pict :: Pict).epoch_set_metadata(i :: Int,
                                           metadata :: Map) :: Pict
){

 Gets metadata registered for an epoch within a pict, or returns a
 @tech{pict} that is like @rhombus(pict) but with the given metadata for
 the given epoch.

 When picts are combined through operations like @rhombus(beside) or
 @rhombus(overlay), the combination's metadata is formed by merging
 metadata maps from the combined picts, and later picts in the
 combination take precedence.

}

@doc(
  method (pict :: Pict).draw(dc :: DC,
                             ~dx: dx :: Real = 0,
                             ~dx: dy :: Real = 0) :: Void
  method (pict :: Pict).drawer()
    :: Function.of_arity(1, ~dx, ~dy)
){

 Draws a @tech{pict} or returns a function that can be used to draw the
 pict. Repeatedly using the function produced by @rhombus(Pict.drawer)
 may be faster than repeatedly calling @rhombus(Pict.draw). If @rhombus(pict)
 is animated, then it is drawn the same as @rhombus(pict.snapshot()).

@examples(
  ~eval: pict_eval
  ~repl:
    def bm = draw.Bitmap([20, 20], ~backing_scale: 2)
    def p = circle(~size: 20, ~fill: "lightgreen")
    p.draw(bm.make_dc())
    ~fake:
      bm.write("circle.png", ~kind: #'png)
      #void
)

}

@doc(
  property (pict :: StaticPict).handle :: Any
  property (pict :: StaticPict).draw_handle :: Any
  fun Pict.from_handle(hand :: Any) :: StaticPict
){

 Converts a rhombus @tech{pict} to/from a Racket pict.

 The @rhombus(StaticPict.draw_handle) property returns a Racket pict
 suitable for drawing directly or embedding in a Racket pict
 construction. It configures the drawing context with more modern
 defaults, including drawing in @rhombus(#'smoothed) mode.

}

@doc(
  enum TimeOrder:
    before
    after
){

 Options for time directions.

}
