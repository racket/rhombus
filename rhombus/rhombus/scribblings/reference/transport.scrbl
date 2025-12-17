#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/network open)

@title(~tag: "transport"){Port Numbers and Waiting Modes}

@doc(
  annot.macro 'PortNumber'
  annot.macro 'ListenPortNumber'
){

 A @rhombus(PortNumber, ~annot) is a @rhombus(Int.in(1 ..= 65535), ~annot). A
 @rhombus(ListenPortNumber, ~annot) is a @rhombus(Int.in(0 ..= 65535), ~annot),
 where @rhombus(0) indicates that the operating system should chose an
 ephemeral port number.

}

@doc(
  enum NetworkWait
  | all
  | enable_break
){

 The @rhombus(NetworkWait, ~annot) enumeration is used by various
 networking functions to enable breaks or not.

}
