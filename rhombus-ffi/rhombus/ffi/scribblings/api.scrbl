#lang rhombus/scribble/manual

@title(~tag: "api", ~style: #'toc){Foreign Types, Pointers, and Functions}

The core functionality provided by @rhombusmodname(ffi) covers loading
foreign libraries dynamically, describing foreign types, working with
pointers to foreign data, and calling foreign functions or having
foreign functions call back to Rhombus.

@local_table_of_contents()

@include_section("lib.scrbl")
@include_section("base-type.scrbl")
@include_section("compound-type.scrbl")
@include_section("define-type.scrbl")
@include_section("pointer.scrbl")
@include_section("function.scrbl")
