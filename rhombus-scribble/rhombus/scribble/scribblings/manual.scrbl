#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: [#'toc], ~tag: "manual"){Documentation Scribble}

@docmodule(~lang, rhombus/scribble/manual)

The @rhombuslangname(rhombus/scribble/manual) language follows the same
general rules as @rhombuslangname(rhombus/scribble), and provides all of
the bindings from that language. In addition,
@rhombuslangname(rhombus/scribble/manual) provides bindings for
typesetting and documenting Rhombus programs.

@local_table_of_contents()

@include_section("rhombus.scrbl")
@include_section("doc.scrbl")
@include_section("manual-text.scrbl")
@include_section("examples.scrbl")
@include_section("spacer.scrbl")
@include_section("doc_meta.scrbl")
