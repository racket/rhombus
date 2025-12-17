#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open      
      draw.Font)

@title(~category: #'tool){Slideshow: Presentation Tool}

@docmodule(slideshow)

The @rhombusmodname(slideshow) Rhombus library is built on the Racket
@racketmodname(slideshow) library, but uses the Rhombus
@rhombusmodname(pict) library's picts and @rhombusmodname(pict/text)
utilities for text layout. The @rhombusmodname(slideshow) module
re-exports @rhombusmodname(pict) and @rhombusmodname(pict/text).

@table_of_contents()

@include_section("overview.scrbl")

@include_section("slide.scrbl")
