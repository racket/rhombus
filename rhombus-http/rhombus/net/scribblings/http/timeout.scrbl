#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http
      net/http open
      net/url)

@title(~tag: "timeout"){Timeout Configuration}

@doc(
  class http.Timeouts():
    constructor (
      ~lease: lease :: maybe(PosReal) = 5,
      ~connect: connect :: maybe(PosReal) = 5,
      ~request: request :: maybe(PosReal) = 30
    )
){

 Configures timeouts for @rhombus(Session.request).

}
