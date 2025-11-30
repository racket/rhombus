#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: [#'toc], ~tag: "base"){Core Scribble}

@docmodule(~lang, rhombus/scribble)

The general rules of the @rhombuslangname(rhombus/scribble) language are
described in @secref("overview"). This section describes all of the
bindings provided by the language.

@local_table_of_contents()

@include_section("section.scrbl")
@include_section("block.scrbl")
@include_section("element.scrbl")
@include_section("image.scrbl")
@include_section("spacing.scrbl")
@include_section("link.scrbl")
@include_section("index.scrbl")
@include_section("toc.scrbl")
@include_section("annot.scrbl")
@include_section("more_annot.scrbl")
