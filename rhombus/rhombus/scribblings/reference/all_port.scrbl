#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc, ~tag: "port"){Ports}

A @deftech{port} is an input or output stream for a file, network
connection, terminal, etc. An @tech{input port} is specifically for
input, while an @tech{output port} is specifically for
output; it is possible for an object to be both an input and output port.

See @secref(~doc: model_doc, "port") in @docref(model_doc) for more
information about ports.

@local_table_of_contents()

@include_section("port.scrbl")
@include_section("input_port.scrbl")
@include_section("output_port.scrbl")
@include_section("file_stream_port.scrbl")
@include_section("pipe.scrbl")
@include_section("port-lib.scrbl")
