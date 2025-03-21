#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "payload"){Payload Data}

@doc(
  annot.macro 'payload.Function'
){

 Equivalent to

@rhombusblock(
  (Headers)
    -> values(Headers, Bytes || String || Port.Input)
)

 representing a function to generate a payload for @rhombus(Session.request).

}

@doc(
  fun payload.buffered(f :: payload.Function) :: payload.Function
){

 Produces a payload function that buffers the result of @rhombus(f) in
 memory to determine its length before sending it to the server.

}

@doc(
  fun payload.form(v :: List.of(url.KeyValue)) :: payload.Function
){

 Produces a payload function that encodes @rhombus(v) as form data using the
 @rhombus(application/x-www-form-urlencoded) content type.

}

@doc(
  fun payload.json(v :: JSON) :: payload.Function
){

 Produces a payload function that encodes @rhombus(v) as JSON data.

}


@doc(
  fun payload.gzip(f :: payload.Function) :: payload.Function
){

 Produces a payload function that gzips the output of @rhombus(f).

}

@doc(
  fun payload.pure(inp :: Bytes || String || Port.Input)
    :: payload.Function
){

 Produces a payload function that uses @rhombus(inp) as the request body.

}
