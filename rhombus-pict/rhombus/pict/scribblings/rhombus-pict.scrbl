#lang rhombus/scribble/manual

@(import:
    meta_label:
      pict open
      draw
    "pict_eval.rhm" open)

@title(~category: #'#{gui-library}){Pict: Functional Pictures}

@docmodule(pict)

The @rhombusmodname(pict) Rhombus library starts with the same
@deftech{pict} concept as the Racket @racketmodname(pict) library, but
the Rhombus pict abstraction includes direct support for slideshow
animations. That is, the @rhombus(Pict) datatype from the
@rhombusmodname(pict) library covers both static pictures, as useful in
a paper, and dynamic pictures, as useful in a slide presentation. In
particular, a @rhombus(Pict, ~annot) can represent an entire Slideshow talk.
Concretely, you can pass an animated pict to a function like
@rhombus(above), and the result will be an animated pict that animates
and steps concurrent to other animated picts provided in the same
@rhombus(above) combination.

@table_of_contents()

@include_section("overview.scrbl")
@include_section("pict.scrbl")
@include_section("shape.scrbl")
@include_section("combine.scrbl")
@include_section("animate.scrbl")
@include_section("rebuild.scrbl")
@include_section("find.scrbl")
@include_section("text.scrbl")
@include_section("radial.scrbl")
@include_section("balloon.scrbl")
@include_section("rhombus.scrbl")

@close_eval(pict_eval)
