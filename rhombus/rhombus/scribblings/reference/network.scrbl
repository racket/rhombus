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

@doc(
  annot.macro 'PortNumber'
  annot.macro 'ListenPortNumber'
  enum NetworkWait:
    all
    enable_break
){

 A @rhombus(PortNumber, ~annot) is a @rhombus(Int.in(1, 65535)). A
 @rhombus(ListenPortNumber, ~annot) is a @rhombus(Int.in(0, 65535)),
 where @rhombus(0) indicates that the operating system should chose an
 ephemeral port number.

 The @rhombus(NetworkWait, ~annot) enumeration is used by various
 networking functions to enable breaks or not.

}

@include_section("tcp.scrbl")
@include_section("udp.scrbl")
