#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      pict open)

@title(~tag: "nothing"){The Nothing Pict}

The constant @rhombus(nothing) is a @tech{static pict} that behaves in
general as if it isn't there. It has no drawing, and it has a width and
height of @rhombus(0). Attempting to pad the @rhombus(nothing) pict will
not change the bounding box; the result is still @rhombus(nothing) with
width and height @rhombus(0). When @rhombus(beside) or @rhombus(stack)
is called on zero arguments (or with all @rhombus(nothing) arguments),
the result is @rhombus(nothing). When @rhombus(nothing) is an argument
to @rhombus(beside) or @rhombus(stack) alongside a non-@rhombus(0) value
for @rhombus(~sep), then no extra separator space is included at the
place where @rhombus(nothing) is provided.

The @rhombus(Pict.time_clip) operation prunes an animated pict's
timeline by making it render as @rhombus(nothing) outside of its
@tech{time box}. This can be useful, for example, to prevent a pict from
participating in alignment calculations outside of its time box.
