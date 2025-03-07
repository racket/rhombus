#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Rhombus}

Rhombus is a general-purpose programming language that is easy to use for
everyday tasks, but also uniquely customizable for domain-specific tasks.

Rhombus is built on @hyperlink("https://racket-lang.org"){Racket}, and
it is extensible in the same way as Racket, but Rhombus uses a more
conventional expression syntax. No familiarity with Racket is required
to use Rhombus.

This document is a general guide to Rhombus. For complete specification,
see @docref(model_doc), @docref(ref_doc), and @docref(meta_doc).

@table_of_contents()

@include_section("guide/overview.scrbl")
@include_section("guide/datatype.scrbl")
@include_section("guide/macro.scrbl")
@include_section("guide/class-overview.scrbl")
@include_section("guide/static-overview.scrbl")
@include_section("guide/running.scrbl")
