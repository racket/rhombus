#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open      
      draw.Font)

@title(~category: #'tool){Slideshow: Presentation Tool}

@docmodule(slideshow, slideshow/content)

Slideshow is a library for creating presentation slides. Unlike
PowerPoint, Slideshow provides no WYSIWYG interface for constructing
slides. Instead, a presentation is generated and run as a program.

The @rhombusmodname(slideshow) Rhombus library builds on the
@rhombusmodname(pict) and @rhombusmodname(pict/text) libraries, and
@rhombusmodname(slideshow) re-exports all of @rhombusmodname(pict) and
@rhombusmodname(pict/text). The @rhombusmodname(slideshow/content) library
exports just the bindings defined in this manual, and not
@rhombusmodname(pict) and @rhombusmodname(pict/text). Internally,
Rhombus Slideshow builds on the Racket @racketmodname(slideshow)
library.

@table_of_contents()

@include_section("overview.scrbl")

@include_section("slide.scrbl")
@include_section("config.scrbl")
@include_section("viewer.scrbl")

@include_section("running.scrbl")
