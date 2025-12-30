#lang rhombus/scribble/manual

@title(~category: #'#{gui-library}){Draw: Drawing Toolkit}

@docmodule(~use_sources: lib("rhombus/draw.rhm"),
           draw)

The @rhombusmodname(draw) library aprovides a drawing API that is based
on the PostScript drawing model. It supports line drawing, shape
filling, bitmap copying, alpha blending, and affine transformations
(i.e., scale, rotation, and translation).

@table_of_contents()

@include_section("overview.scrbl")
@include_section("state.scrbl")
@include_section("draw.scrbl")
@include_section("dcs.scrbl")
@include_section("object.scrbl")
@include_section("point-et-al.scrbl")
