#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style", ~style: #'toc){Styles and Style Properties}

@docmodule(~hidden, ~lang, rhombus/scribble)

@local_table_of_contents()

@include_section("style-elem.scrbl")
@include_section("style-part.scrbl")
@include_section("style-para.scrbl")
@include_section("style-table.scrbl")
@include_section("style-itemization.scrbl")
@include_section("style-nested-flow.scrbl")
@include_section("style-compound-para.scrbl")

@include_section("style-property-annot.scrbl")
