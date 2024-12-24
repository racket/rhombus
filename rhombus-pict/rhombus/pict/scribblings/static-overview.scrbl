#lang rhombus/scribble/manual

@(import:
    pict open
    draw
    meta_label:
      rhombus open
      pict open
      draw
    "pict_diagram.rhm".pict_diagram
    "pict_eval.rhm" open
    "timeline.rhm" open)

@title(~tag: "static-pict"){Static Picts}

A @deftech{static pict} is a pict like one from Racket's
@racketmodname(pict) library. It has a width, height, and a rendering as
an image. A pict does not have a particular location on the page or
screen. Instead, different picts are combined relative to each other to
form a larger pict, and then the overall pict can be included in a
document or used as slide content.

@examples(
  ~eval: pict_eval
  def rect = rectangle(~width: 40, ~height: 20, ~fill: "lightblue",
                       ~line: #'inherit)
  rect
  def circ = circle(~size: 40, ~fill: "lightgreen")
  beside(rect, circ)
)

Some parts of a pict's drawing may use an inherited color or line width
that can be adjusted by creating a new pict that overrides the color or
line width:

@examples(
  ~eval: pict_eval
  rect.colorize("red")
)

More generally, since picts often represent text that needs to be
positioned along a shared baseline, a static pict has a width @math{w},
height @math{h}, ascent @math{a}, and descent @math{d}:

@centered(pict_diagram)

In a pict representing a single line of text, the ascent plus descent
will match the total height of the picture. For multiple lines of text,
the ascent reflects the top line's baseline, and the descent represents
the bottom line's baseline.

@examples(
  ~eval: pict_eval
  def prolog = text("We typically use ").colorize("brown"),
  def big = draw.Font(~size: 24)
  def hello = text("Hello", ~font: big).colorize("forestgreen")
  def world = text("World", ~font: big).colorize("firebrick")
  def epilog = text(" as an example").colorize("blue"),
  beside(~vert: #'top,
         prolog,
         beside(~vert: #'bottom,
                stack(~horiz: #'left, hello, world),
                epilog))
  beside(~vert: #'topline,
         prolog,
         beside(~vert: #'baseline,
                stack(~horiz: #'left, hello, world),
                epilog))
)

The width and height components of a pict are called its
@deftech{bounding box}, but a pict's drawing is not actually constrained
to its bounding box. Instead, a pict's bounding box is used for
positioning it relative to other picts, and shifting a pict's drawing
relative to its bounding box can be useful for composing images. The
@rhombus(Pict.pad) function, when given positive padding, grows a pict's
bounding box without affecting the drawn image. The larger bounding box
can affect how a pict is centered with respect to another one:

@examples(
  ~eval: pict_eval
  beside(rect, circ)
  beside(rect.pad(~top: 15, ~right: 5), circ)
)

Negative padding shrinks a bounding box, which may cause a pict's
drawing to extend past the bounding box. For example, stacking a circle
and line above a rectangle creates a pict with a bounding box whose
height matches the sum of the stacked pictures, but we can subtract back
off the height of the circle and line, so that the rectangle aligns as
it did before the addition:

@examples(
  ~eval: pict_eval
  fun antenna(p):
    let ap = stack(circle(~size: 4), line(~dy: 6, ~line_width: 2), p)
    // negative cancels circle+line contribution to bounding box:
    ap.pad(~top: -10)
  beside(antenna(rect), circ)
)

This kind of adjustment---shifting a compound pict's bounding box so
that it surrounds just one of the components---is so common that there's
a shorthand operation to do it, @rhombus(Pict.refocus). Like all
bounding-box operations, refocusing adjusts the resulting pict's descent
and height, too. (Refocusing relies on the fact that a pict can be found
within a composite pict, as we explain a little further below.)

@examples(
  ~eval: pict_eval
  ~repl:
    fun antenna(p):
      let ap = stack(circle(~size: 4), line(~dy: 6, ~line_width: 2), p)
      ap.refocus(p)
    beside(antenna(rect), circ)
  ~repl:
    fun underline(p :: Pict):
      stack(p, line(~dx: p.width))
    beside(~vert: #'baseline, hello, underline(world))
  ~repl:
    fun underline(p :: Pict):
      stack(p, line(~dx: p.width)).refocus(p)
    beside(~vert: #'baseline, hello, underline(world))
  ~repl:
    beside(~vert: #'baseline,
           hello,
           // adding a rectangle around a pict and refocusing is so
           // commonly useful that `rectangle` builds in shortcuts:
           rectangle(~around: world.pad(2), ~refocus: world))
)

There's not a way, in general, to determine the full extent of a pict's
drawing. Contexts that render a pict, such as this documentation, used
the bounding box as a guide for how much to draw when rendering a single
pict. This documentation's rendering allows a small mount of drawing
past the bounding box, but clips beyond that small allowance.

@examples(
  ~eval: pict_eval
  hello.pad(-10)
)

Pict identities are preserved in a composite pict so that the originals
can be located relative to the composite. The @rhombus(Find, ~annot)
familty of functions produces a pict-specific finder that can be applied
to a composite pict.

@examples(
  ~eval: pict_eval
  def comp = beside(rect, circ)
  comp
  Find.left(rect).in(comp)
  Find.left(circ).in(comp)
)

The @rhombus(Pict.ghost) operation takes a pict and produces a new one
whose drawing is blank, but with the same bounding box and preserving
the identities of all picts (including the ghosted one). The
@rhombus(Pict.launder) operation on a pict produces one that draws the
same and has the same bounding box, but that hides the identity of all
picts within the composite, so @rhombus(Find, ~annot) functions cannot find
them.

Not only are pict identities preserved in a composite pict, but the way
that those picts are used to produce the composite is also recorded.
Consequently, a pict supports a replacement operation that replays the
pict's construction, but using a given pict in place of an original.

@examples(
  ~eval: pict_eval
  comp
  comp.replace(circ, ellipse(~width: 60, ~height: 30, ~fill: "orange"))
)

The @rhombus(Pict.replace) operation is a special case of
@rhombus(Pict.rebuild), which accepts a function that gets the
opportunity to adjust each pict that was part of a given pict's
construction.
