#lang rhombus/scribble/manual

@(import:
    meta_label:
      pict open
      draw
    "pict_eval.rhm" open)

@(def scribble_doc:
    ModulePath'lib("rhombus/scribble/scribblings/rhombus-scribble.scrbl")')

@(def slideshow_doc:
    ModulePath'lib("rhombus/slideshow/scribblings/rhombus-slideshow.scrbl")')

@title(~category: #'#{gui-library}){Pict: Functional Pictures}

@docmodule(pict)

The @rhombusmodname(pict) library for Rhombus supports functional
construction of 2-D animated pictures. A @deftech{pict} (satisfying
@rhombus(Pict, ~annot)) can implement a static picture, as might appear
in a paper, or an animated sequence, as might appear in a slide
presentation. A pict can be drawn to the screen, saved to a bitmap or
vector-graphics file, incorporated into a
@seclink(~doc: scribble_doc, "top", ~indirect: #true){Scribble}
document, or used in a
@seclink(~doc: slideshow_doc, "top", ~indirect: #true){Slideshow} talk.

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
