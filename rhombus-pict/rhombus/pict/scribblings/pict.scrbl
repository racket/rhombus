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
  annot.macro 'StaticPict'
){

 An annotation representing a @tech{pict} value or a pict value that is
 specifically a @tech{static pict}.

}

@doc(
  def nothing :: NothingPict
  annot.macro 'NothingPict'
){

 The @rhombus(nothing) pict is a special @tech{static pict} that acts as
 if it is not supplied at all. The @rhombus(NothingPict, ~annot) annotation
 is satisfied by only @rhombus(nothing).

}

@doc(
  property (pict :: Pict).width :: Real
  property (pict :: Pict).height :: Real
  property (pict :: Pict).descent :: Real
  property (pict :: Pict).ascent :: Real
){

 Properties for a @tech{pict}'s geometry.

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
 may be faster than repeatedly calling @rhombus(Pict.draw). If the pict
 is animated, then it is drawn the same as @rhombus(Pict.snapshot(pict)).

}

@doc(
  property (pict :: Pict).duration :: Int
  method (pict :: Pict).epoch_extent(i :: Int) :: Real
){

 Properties for a @tech{pict}'s animation.

}

@doc(
  property (pict :: Pict).children :: List.of(Pict)
){

 A list of component @tech{picts} that were combined to construct
 @rhombus(pict). Those picts can be located within @rhombus(pict) using a
 @tech{finder}.

}

@doc(
  method (pict :: Pict).snapshot() :: StaticPict
  method (pict :: Pict).snapshot(epoch :: Int, n :: Real.in(0, 1))
    :: StaticPict
){

 Converts an @tech{animated pict} to a @tech{static pict}. The 0-argument
 variant is a shorthand for providing @rhombus(0) and @rhombus(0).

}

@doc(
  method (pict :: Pict).launder() :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with a
 fresh identity and hiding the identity of any component inside
 @rhombus(pict) from a @tech{finder} or the result of the
 @rhombus(Pict.children) property.

}

@doc(
  method (pict :: Pict).ghost(do_ghost = #true) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), including the
 same @tech{bounding box} and @tech{time box}, but whose drawing is empty
 if @rhombus(do_ghost) is true. If @rhombus(do_ghost) is @rhombus(#false),
 then @rhombus(pict) itself is returned.

 The @rhombus(do_ghost) argument is intended to help avoid @rhombus(if)
 wrappers, enabling @rhombus(pict.ghost(@rhombus(test, ~var))) instead of
 @rhombus(if @rhombus(test, ~var) | pict.ghost() | pict), where the
 former works even without having to bind an intermediate variable if
 @rhombus(pict) is replaced with a more complex expression.

}

@doc(
  method (pict :: Pict).refocus(subpict :: Pict) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with a
 shifted @tech{bounding box} to match @rhombus(subpict) within
 @rhombus(pict).

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
  method (pict :: Pict).translate(dx :: Real, dy :: Real) :: Pict
  method (pict :: Pict).lift(amt :: Real) :: Pict
  method (pict :: Pict).drop(amt :: Real) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with its
 @tech{bounding box} adjusted.

}

@doc(
  method (pict :: Pict).replace(orig :: Pict,
                                replacement :: Pict) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction so that @rhombus(replacement) is used
 whenever @rhombus(orig) was originally used. Parts of the @rhombus(pict)
 construction that did not depend on @rhombus(orig) are kept as-is and
 with their existing identities, while replayed operations create fresh
 identities.

 To support @rhombus(Pict.replace), the representation of a pict
 effectively records all primitive operations used to construct the pict.
 This recording is limited to @rhombus(Pict, ~annot) and @rhombus(Find, ~annot) objects.
 If, for example, you use @rhombus(Find.in) to obtain a number and then
 construct a pict using that number, the number itself cannot record its
 derivation from the picts used with @rhombus(Find.in). In such cases,
 use @rhombus(configure) or the @rhombus(~children) argument of
 @rhombus(animate) to establish a connection between the input picts and
 the result.

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
  method (pict :: Pict).configure(key :: !Pict,
                                  val : Any)
    :: Pict
){

 Produces a pict like @rhombus(pict), but replays @rhombus(pict)'s
 construction so that any configuration via @rhombus(configure) that uses
 @rhombus(key) is replaced with one where @rhombus(key) is mapped to
 @rhombus(val). To avoid confusion between children picts and
 configuration keys, a @rhombus(key) is constrained to be a non-pict.

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

}

@doc(
  method (pict :: Pict).sustain(n :: Int = 1) :: Pict
){

 Similar to @rhombus(Pict.time_pad) with @rhombus(~after), but
 @tech{sustains} instead of merely padding.

}

@doc(
  method (pict :: Pict).nonsustaining() :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but where a
 @tech{sustain} operation is treated the same as padding via
 @rhombus(Pict.time_pad).

}

@doc(
  method (pict :: Pict).scale(n :: Real) :: Pict
  method (pict :: Pict).scale(horiz :: Real, vert :: Real) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but its drawing and
 bounding box is scaled.

}

@doc(
  method (pict :: Pict).rotate(radians :: Real) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but its drawing is
 rotated, and its bounding box is extended as needed to enclose the
 rotated bounding box.

}

@doc(
  method (pict :: Pict).shear(x_factor :: Real,
                              y_factor :: Real) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but its drawing is
 sheared. The bounding box is inflated to contain the result. The
 result pict's ascent and descent are the same as @rhombus(pict)'s.

}

@doc(
  method (pict :: Pict).alpha(n :: Real.in(0, 1)) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 changed by reducing the global alpha adjustment.

}

@doc(
  method (pict :: Pict).hflip() :: Pict
  method (pict :: Pict).vflip() :: Pict
){

 Flips a @tech{pict} horizontally or vertically.

}

@doc(
  method (pict :: Pict).clip() :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 confined to its bounding box.

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

}


@doc(
  method (pict :: Pict).colorize(c :: Color || String) :: Pict
  method (pict :: Pict).line_width(w :: NonnegReal) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but wherever it uses
 @rhombus(#'inherit) for a color or line width, the given color or line
 width is used, and the resulting pict no longer uses @rhombus(#'inherit)
 or colors or line widths.

}

@doc(
  method (pict :: Pict).epoch_set_extent(i :: Int,
                                         extent :: NonnegReal) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but with the
 @tech{extent} of one of its @tech{epochs} adjusted.

}

@doc(
  method (pict :: Pict).metadata() :: Map
  method (pict :: Pict).set_metadata(metadata :: Map) :: Pict
){

 Gets metadata registered for an immediate pict, or returns a
 @tech{pict} that is like @rhombus(pict) but with the given metadata.

 Unlike epoch-specific metadata added with
 @rhombus(Pict.epoch_set_metadata), metadata added by
 @rhombus(Pict.metadata) is not propagated to new picts that are derived
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
  enum TimeOrder:
    before
    after
){

 Options for time directions.

}
