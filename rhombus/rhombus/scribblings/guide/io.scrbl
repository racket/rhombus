#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "io", ~style: #'toc){Input, Output, and Strings}

Rhombus's @tech{port} layer supports general I/O in terms of @tech{byte
 strings}, but it also supports strings with an implicit coercion via
UTF-8. Those implicit coercions mesh with Rhombus's string-based
printing, interpolation, and @tech{regexp} matching libraries.

@local_table_of_contents()

@include_section("print.scrbl")
@include_section("string-interpolation.scrbl")
@include_section("port.scrbl")
@include_section("closeable.scrbl")
@include_section("rx.scrbl")
