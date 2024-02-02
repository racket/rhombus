#lang scribble/rhombus/manual

@title{Rhombus GUI}

@docmodule(~use_sources: lib("rhombus/gui.rhm"),
           gui)

The @rhombusmodname(gui) library is based on
@racketmodname(racket/gui/easy).

@table_of_contents()

@include_section("observable.scrbl")
@include_section("renderer.scrbl")
@include_section("view.scrbl")
@include_section("window.scrbl")
@include_section("panel.scrbl")
@include_section("canvas.scrbl")
@include_section("control.scrbl")
@include_section("event.scrbl")
@include_section("annotation.scrbl")
