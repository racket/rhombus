#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "object", ~style: #'toc){Drawing Utensils}

Objects such as @rhombus(Color, ~annot), @rhombus(Pen, ~annot),
@rhombus(Brush, ~annot), and @rhombus(Font, ~annot) instances serve as
values for a @seclink("state"){drawing state} or arguments to a
@seclink("draw"){drawing operation}.

@table_of_contents()

@include_section("color.scrbl")
@include_section("pen.scrbl")
@include_section("brush.scrbl")
@include_section("font.scrbl")
@include_section("region.scrbl")
@include_section("path.scrbl")
