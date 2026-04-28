#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http
      net/http open
      net/url
      json)

@title(~tag: "payload"){Payload Data}

@doc(
  annot.macro 'http.payload.Function'
){

 Equivalent to

@rhombusblock(
  (Headers)
    -> values(Headers, Bytes || String || Port.Input)
)

 representing a function to generate a payload for @rhombus(Session.request).

}

@doc(
  fun http.payload.buffered(f :: payload.Function) :: payload.Function
){

 Produces a payload function that buffers the result of @rhombus(f) in
 memory to determine its length before sending it to the server.

}

@doc(
  fun http.payload.form(v :: List.of(url.KeyValue)) :: payload.Function
){

 Produces a payload function that encodes @rhombus(v) as form data using the
 @tt{application/x-www-form-urlencoded} content type.

}

@doc(
  fun http.payload.json(v :: json.JSON) :: payload.Function
){

 Produces a payload function that encodes @rhombus(v) as JSON data.

}


@doc(
  fun http.payload.gzip(f :: payload.Function) :: payload.Function
){

 Produces a payload function that gzips the output of @rhombus(f).

}

@doc(
  fun http.payload.pure(inp :: Bytes || String || Port.Input)
    :: payload.Function
){

 Produces a payload function that uses @rhombus(inp) as the request body.

}


@doc(
  class http.payload.multipart.Part():
    constructor ~none
  fun http.payload.multipart(
    ~boundary: boundary :: maybe(Bytes || String),
    f :: payload.multipart.Part, ...
  ) :: payload.Function
){

 A @rhombus(payload.multipart.Part, ~class) can be used with @rhombus(payload.multipart)
 to produce a @tt{multipart/form-data} payload.

}

@doc(
  fun http.payload.multipart.Part.field(
    ~name: name :: Bytes || String,
    ~value: value :: Bytes || String,
    ~content_type: content_type :: Bytes || String = #"text/plain"
  ) :: payload.multipart.Part
){

 Produces a @rhombus(payload.multipart.Part, ~class) that encapsulates a form field.

}

@doc(
  fun http.payload.multipart.Part.file(
    ~name: name :: Bytes || String,
    ~in: inp :: Port.Input,
    ~filename: filename :: Bytes || String = to_string(inp.name()),
    ~content_type: content_type :: Bytes || String = #"application/octet-stream"
  ) :: payload.multipart.Part
){

 Produces a @rhombus(payload.multipart.Part, ~class) that encapsulates a file.

}

@doc(
  property (pt :: http.payload.multipart.Part).handle
){

 Returns a Racket representation of the part.

}
