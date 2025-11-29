#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Rhombus Scribble}

Rhombus Scribble is a language for writing prose documents, especially
documentation for Rhombus libraries. The result of running a Rhombus
Scribble program is a document, and a Rhombus Scribble program is meant
to be run with @exec{raco scribble}, which takes the document result and
renders it to a format such as HTML or PDF.

@table_of_contents()

@include_section("getting_started.scrbl")
@include_section("overview.scrbl")
@include_section("base.scrbl")
@include_section("manual.scrbl")
@include_section("library.scrbl")
