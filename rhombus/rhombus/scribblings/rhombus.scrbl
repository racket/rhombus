#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Rhombus}

Rhombus is a general-purpose programming language with conventional
expression syntax that is built on
@hyperlink("https://racket-lang.org"){Racket} and that is
macro-extensible in the same way as Racket.

This document is a general guide to Rhombus. For complete specification,
see @docref(ref_doc).

@table_of_contents()

@include_section("guide/overview.scrbl")
@include_section("guide/datatype.scrbl")
@include_section("guide/macro.scrbl")
@include_section("guide/class-overview.scrbl")
@include_section("guide/static-overview.scrbl")
