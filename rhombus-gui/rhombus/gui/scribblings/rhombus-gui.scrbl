#lang rhombus/scribble/manual

@title(~category: #'#{gui-library}){GUI: Graphical User Interface Toolkit}

@docmodule(~use_sources: lib("rhombus/gui.rhm"),
           gui)

The @rhombusmodname(gui) library provides a reactive framework for
constructing graphical user interface (GUI) applications.

@table_of_contents()

@include_section("overview.scrbl")
@include_section("observable.scrbl")
@include_section("all-view.scrbl")
@include_section("renderer.scrbl")
@include_section("event.scrbl")
@include_section("annotation.scrbl")
