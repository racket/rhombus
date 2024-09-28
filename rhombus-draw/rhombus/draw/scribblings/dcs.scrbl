#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc){Drawing Contexts}

Every drawing context is an instance of the @rhombus(DC, ~class) interface.

@local_table_of_contents()

@include_section("dc.scrbl")
@include_section("bitmapdc.scrbl")
@include_section("pageddc.scrbl")
@include_section("psdc.scrbl")
@include_section("pdfdc.scrbl")
@include_section("svgdc.scrbl")
