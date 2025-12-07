#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc, ~tag: "running"){Building and Running Rhombus Programs}

Rhombus inherits Racket's tool for editing and running programs.

@local_table_of_contents()

@include_section("editor.scrbl")
@include_section("running-cmdline.scrbl")
@include_section("repl.scrbl")
@include_section("main-submod.scrbl")
@include_section("standalone.scrbl")
@include_section("cmdline.scrbl")
