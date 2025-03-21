#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "timeout"){Timeout Configuration}

@doc(
  class Timeouts(_handle):
    constructor (
      ~lease: lease :: maybe(PosReal) = 5,
      ~connect: connect :: maybe(PosReal) = 5,
      ~request: request :: maybe(PosReal) = 30
    )
){

 Configures timeouts for @rhombus(Session.request).

}
