#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/network open)

@title(~style: #'toc, ~tag: "network"){Networking}

@docmodule(rhombus/network)

Rhombus's core networking facilities are provided by the
@rhombusmodname(rhombus/network) module.

@local_table_of_contents()

@include_section("tcp.scrbl")
@include_section("udp.scrbl")
@include_section("transport.scrbl")
