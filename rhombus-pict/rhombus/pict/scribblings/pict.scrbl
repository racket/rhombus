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
  method (pict :: Pict).alpha(
    n :: Real.in(0, 1),
    ~composite: composite = #true
  ) :: Pict
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

 But default, all drawn elements of @rhombus(pict) are rendered
 together, and then an alpha adjustment is applied to the combined
 drawing. If @rhombus(composite) is @rhombus(#false), then individual
 elements of @rhombus(pict) are drawn with an alpha adjustment. When the
 two modes produce different output, the default one is normally the
 intended effect, but drawing with @rhombus(composite) as
 @rhombus(#false) can be significantly faster in some cases.

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

 Produces the @tech{findable children} pf @rhombus(pict): a list of component
 @tech{picts} that were combined to construct @rhombus(pict). Those picts
 can be located within @rhombus(pict) using a @tech{finder}.

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
  method (pict :: Pict).launder(
    ~rebuild_prompt: rebuild_prompt = #false
  ) :: Pict
){

 Returns a @tech{pict} that is the same as @rhombus(pict), but with a
 fresh identity and hiding the identity of any component inside
 @rhombus(pict) from a @tech{finder} or the result of the @rhombus(Pict.children)
 property. That is, the result from @rhombus(Pict.launder) has no
 @tech{findable children} other than itself.

 If @rhombus(rebuild_prompt) is true, then the result pict also has no
 @tech{replaceable dependencies}.

 See also @secref("identity") and @rhombus(Pict.blank).

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

 See also @rhombus(Pict.blank).

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
  method (pict :: Pict).blank(
    ~rebuild_prompt: rebuild_prompt = #true
  ) :: Pict
){

 Equivalent to
 @rhombus(pict.ghost().launder(~rebuild_prompt: rebuild_prompt)) --- where
 @rhombus(rebuild_prompt) defaults to @rhombus(#true), in contrast to the
 @rhombus(#false) default for @rhombus(Pict.launder).

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
  method (pict :: Pict).freeze(~scale: scale :: Real = 2.0) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but whose drawing is
 rendered at the time of the @rhombus(Pict.freeze) call to a bitmap, and
 then the new pict draws by drawing the bitmap. The @rhombus(scale)
 argument determines the scale factor of the bitmap (i.e., the number of
 pixels per drawing unit).

}

@doc(
  method (pict :: Pict).paragraph_end_bounds()
    :: values(Real, Real, Real, Real, Real, Real)
  method (pict :: Pict).set_paragraph_end_bounds(
    dx :: Real,
    dy :: Real,
    width :: Real,
    height :: Real,
    ascent :: Real,
    descent :: Real
  ) :: Pict
){

 Gets or sets bounds relative to @rhombus(pict)'s @tech{bounding box}
 that specify a nested @tech{bounding box} for composition via
 @rhombus(beside) in @rhombus(#'paragraph) attach mode. The results from
 @rhombus(Pict.paragraph_end_bounds) are in the same order as the
 arguments to @rhombus(Pict.set_paragraph_end_bounds), which describe an
 offset and a bounding box at that offset.

 For most pict constructions, the paragraph-end bounding box will be the
 same as the pict's overall bounding box with a zero offset.

 Whenever multiple picts are combined using @rhombus(stack) or
 @rhombus(beside), the paragraph-end bounding box of the last pict
 provided to @rhombus(stack) or @rhombus(beside) is used for the result
 pict's paragraph-end bounds. A @rhombus(beside) combination in
 @rhombus(#'paragraph) attach mode will then append relative to that last
 pict in the composition, instead of to the composition as a whole.

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
  property (pict :: Pict).identity :: PictIdentity
  class PictIdentity():
    ~no_constructor
  method (ident :: PictIdentity).description() :: List.of(String)
  method (pict :: Pict).set_description(desc :: Any) :: Pict
  method (pict :: Pict).description() :: List.of(String)
){

 The @rhombus(Pict.identity) property returns an object representing the
 identity of a pict for the purpose of finding it in other picts. See
 @secref("identity") for more information about pict identity.

 An identity has a description that is used by @rhombus(to_string) for a
 pict, which can be helpful for debugging. A pict's description is
 normally synthesized automatically, but it can be configured through
 @rhombus(Pict.set_description). A description provided to
 @rhombus(Pict.set_description) is most useful as a string, another
 pict's identity, a list of such values, or @rhombus(#false) to suppress
 a debugging description.

 The @rhombus(Pict.description) method is a shorthand for getting the
 description of @rhombus(pict)'s identity.

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
