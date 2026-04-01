#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http
      net/http open
      net/url)

@title(~tag: "exn"){Exceptions}

@doc(
  veneer http.Exn.Fail.HTTP
  veneer http.Exn.Fail.HTTP.Timeout
  property (exn :: http.Exn.Fail.HTTP.Timeout).kind
    :: Any.of(#'lease, #'connect, #'request)
){

 Veneers to recognize exceptions thrown by @rhombus(Session.request) and
 related functions.

}
