#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "io-model", ~style: #'toc){Input and Output Model}

Rhombus exposes the host operating's input and output facilities mostly
in the form of @tech{port} objects.

@local_table_of_contents()

@include_section("port.scrbl")
@include_section("encoding.scrbl")
@include_section("port-buffer.scrbl")
@include_section("port-line-counting.scrbl")
@include_section("path.scrbl")
