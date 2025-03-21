#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "exn"){Exceptions}

@doc(
  veneer Exn.Fail.HTTP
  veneer Exn.Fail.HTTP.Timeout
  property (exn :: Exn.Fail.HTTP.Timeout).kind
    :: Any.of(#'lease, #'connect, #'request)
){

 Veneers to recognize exceptions raised by @rhombus(Session.request) and
 related functions.

}
