#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "overview", ~style: #'toc){Overview}

The process of turning a Scribble program into a rendered document (such
as HTML or PDF) involves several pieces: the notation used for the
program, the underlying structure that represents a document, the way
that text elements are decoded into that structure, and the passes the
resolve cross-references and ultimately produce the rendered output
document.

@local_table_of_contents()

@include_section("notation.scrbl")
@include_section("structure.scrbl")
@include_section("decoding.scrbl")
@include_section("passes.scrbl")
@include_section("renderer.scrbl")
