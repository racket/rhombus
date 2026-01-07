#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "dcs", ~style: #'toc){Drawing Contexts}

Every drawing context is an instance of the @rhombus(DC, ~class)
interface. A drawing context can be provided by a canvas from the
@rhombusmodname(gui) library, created as a @rhombus(Bitmap, ~class) plus
a call to its @rhombus(Bitmap.make_dc) method, or created as paged
output via @rhombus(PDFCS), @rhombus(PSDC), or @rhombus(SVGDC).

@local_table_of_contents()

@include_section("dc.scrbl")
@include_section("bitmap.scrbl")
@include_section("pageddc.scrbl")
@include_section("recorddc.scrbl")
