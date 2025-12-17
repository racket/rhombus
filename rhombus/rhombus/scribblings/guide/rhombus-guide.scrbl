#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(
  ~category: [#'core, 19]
){Rhombus Guide}

Rhombus is a general-purpose programming language that is easy to use
and uniquely customizable.

@margin_note_block{Rhombus is built on @hyperlink("https://racket-lang.org"){Racket}, and
it is extensible in the same way as Racket, but Rhombus uses a more
conventional expression syntax. No familiarity with Racket is required
to use Rhombus.}

This document is a general guide to the language. For more
documentation, see @docref(getting_started_doc).

@table_of_contents()

@include_section("quick.scrbl")
@include_section("overview.scrbl")
@include_section("datatype.scrbl")
@include_section("class-overview.scrbl")
@include_section("io.scrbl")
@include_section("macro.scrbl")
@include_section("annotation-overview.scrbl")
@include_section("lang.scrbl")
@include_section("running.scrbl")
@include_section("racket.scrbl")
