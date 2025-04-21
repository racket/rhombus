#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      json)

@(def json_eval = make_rhombus_eval())
@examples(
  ~eval: json_eval
  ~hidden:
    import json
)

@title{Rhombus JSON}

@docmodule(json)

The @rhombusmodname(json) library provides functions to read and write
using the JSON exchange format. See See the @hyperlink("http://json.org/"){JSON web site}
and the @hyperlink("http://www.ietf.org/rfc/rfc8259.txt"){JSON RFC} for more information about JSON.

@doc(
  annot.macro 'json.JSON'
){

 The @rhombus(json.JSON, ~annot) annotation matches a subset of Rhombus
 built-in datatypes that can be read and written in JSON format:

@itemlist(

 @item{@rhombus(#true) and @rhombus(#false)}

 @item{@rhombus(String, ~annot) values}

 @item{@rhombus(Int, ~annot) values}

 @item{@rhombus(Flonum, ~annot) values other than @rhombus(#inf),
  @rhombus(#neginf), and @rhombus(#nan) (i.e., @rhombus(Flonum && Real, ~annot))}

 @item{@rhombus(List.of(json.JSON), ~annot) values}

 @item{@rhombus(Map.of(String, json.JSON), ~annot) values}

 @item{@rhombus(#'null)}

)

@examples(
  ~eval: json_eval
  ~repl:
    { "a": 1, "b": [2, 3.0, #true] } is_a json.JSON
    { #'a: 1 } is_a json.JSON
)

 To check whether a list or map satisfies @rhombus(json.JSON, ~annot),
 the list or map's content must be traversed recursively. However, a
 @rhombus(#true) result is cached through a weak reference, so checking
 again for the same (in the sense of @rhombus(===)) list or map produces
 @rhombus(#true) immediately. The result for any value within the list or
 map is similarly cached.

}

@doc(
  fun json.from_string(
    str :: String,
    ~replace_malformed_surrogate: replacement :: Any.to_boolean = #false
  ) :: json.JSON
  fun json.from_bytes(
    bstr :: Bytes,
    ~replace_malformed_surrogate: replacement :: Any.to_boolean = #false
  ) :: json.JSON
  fun json.read(
    ~in: inp :: Port.Input = Port.Input.current(),
    ~replace_malformed_surrogate: replacement :: Any.to_boolean = #false
  ) :: json.JSON || Port.Input.EOF
){

 Parses a JSON representation to a @rhombus(json.JSON, ~annot) value.

 For @rhombus(json.from_string) and @rhombus(json.from_bytes), the
 string or byte string must contain a single JSON encoding.

@examples(
  ~eval: json_eval
  ~repl:
    json.from_string("1")
    json.from_string("{\"a\": [1, 2]}")
    ~error:
      json.from_string("1 2")
    ~error:
      json.from_string("")
    json.from_bytes(#"{\"a\": [1, 3.0, null, false]}")
)

 The @rhombus(json.read) function reads the next JSON encoding from
 @rhombus(inp), stopping (and leaving remaining bytes in the port intact)
 as soon as it finds a complete JSON value; @litchar{true},
 @litchar{false}, or @litchar{null} must be followed by an end-of-file or
 a non-alphanumeric character (such as whitespace). If no JSON
 representation is present in @rhombus(inp) before an end-of-file, the
 result is @rhombus(Port.Input.eof).

@examples(
  ~eval: json_eval
  ~repl:
    def inp = Port.Input.open_string("1 true[\"a\", {\"b\": 3.0}]")
    json.read(~in: inp)
    json.read(~in: inp)
    json.read(~in: inp)
    json.read(~in: inp)
)


 If @rhombus(replacement) is true, then an unpaired surrogate escape in
 an input JSON string representation is replaced with
 @rhombus(Char"\uFFFD"). Otherwise, an unpaired surrogate is treated as
 an input error.

@examples(
  ~eval: json_eval
  ~version_and_later "8.16.0.1":
    ~repl:
      json.from_string(@str{"\u03BB"})
      ~error:
        json.from_string(@str{"\uD870"})
      json.from_string(@str{"\uD870"},
                       ~replace_malformed_surrogate: #true)
)

}


@doc(
  fun json.to_string(j :: json.JSON) :: String
  fun json.to_bytes(j :: json.JSON) :: Bytes
  fun json.write(
    j :: json.JSON,
    ~out: outp :: Port.Output = Port.Output.current()
  ) :: Void
){

 Produces a JSON representation of a @rhombus(json.JSON, ~annot) value.

 The @rhombus(json.to_string) and @rhombus(json.to_bytes) functions
 return the JSON representation as a string or byte string, while
 @rhombus(json.write) prints that representation to an output port.

@examples(
  ~eval: json_eval
  ~repl:
    json.to_string(1)
    json.to_string({ "a": 1, "b": [2, 3.0, #'null, #false] })
    json.to_bytes(1)
    json.write([2, 3, 4])
    block:
      let outp = Port.Output.open_string()
      json.write([2, 3, 4], ~out: outp)
      json.write({ "a": 0 }, ~out: outp)
      outp.get_string()
)

}
