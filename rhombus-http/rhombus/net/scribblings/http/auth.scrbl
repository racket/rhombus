#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "auth"){Authentication}

@doc(
  annot.macro 'auth.Function'
){

 Equivalent to

@rhombusblock(
  (url.URL, Headers, List.of(url.KeyValue))
    -> values(Headers, List.of(url.KeyValue))
)

 representing an authorization function to be supplied to @rhombus(Session.request).

}

@doc(
  fun auth.basic(
    ~username: username :: String || Bytes,
    ~password: password :: String || Bytes
  ) :: auth.Function
){

 Generates a function that authenticates requests using HTTP basic
 authentication.

}

@doc(
  fun auth.bearer(
    ~token: token :: String || Bytes
  ) :: Function
){

 Generates a function that authenticates requests using the given bearer
 token.

}
