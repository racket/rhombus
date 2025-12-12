#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/url open)

@(def rfc_name = "RFC 3986")
@(def url_rfc = @hyperlink("http://www.ietf.org/rfc/rfc3986.txt"){@rfc_name})

@title(~style: #'toc){Rhombus URL and Form Parsing}

@docmodule(net/url)

The @rhombusmodname(net/url) library provides functions for parsing and
manipulating URLs, as well as functions for HTML form and URI component
encoding and decoding.

@table_of_contents()

@section(~tag: "url"){URL Parsing}

@doc(
  class URL(
    ~scheme: scheme :: maybe(String) = #false,
    ~user: user :: maybe(String) = #false,
    ~host: host :: maybe(String) = #false,
    ~port: port :: maybe(Nat) = #false,
    ~is_path_absolute: is_path_absolute :: Boolean = #false,
    ~path: path :: List.of(PathWithParams) = [],
    ~query: query :: List.of(KeyValue) = [],
    ~fragment: fragment :: maybe(String) = #false
  )
  class PathWithParams(
    path :: String || Path.Dot,
    params :: List.of(String)
  )
  class KeyValue(
    key :: String,
    value :: maybe(String)
  )
){

 A class to represent URLs as defined by @url_rfc. The
 following diagram illustrates the parts:

@verbatim(~indent: 2)|{
  http://sky@www:801/cgi-bin/finger;xyz?name=shriram;host=nw#top
  {1-}   {2} {3} {4}{---5---------} {6} {----7-------------} {8}

  1 = scheme, 2 = user, 3 = host, 4 = port,
  5 = path (two elements),  6 = param (of second path element),
  7 = query, 8 = fragment
}|

 The strings inside the @rhombus(user), @rhombus(path), @rhombus(query),
 and @rhombus(fragment) fields are represented without
 URL-syntax-specific quoting. The @rhombus(URL.from_string) function and
 @rhombus(URL.to_string) method translate encodings, such as converting a
 @litchar{%20} into a space and back again.

 By default, query associations are parsed with either @litchar{;} or
 @litchar{&} as a separator, and they are generated with @litchar{&} as a
 separator. The @rhombus(form.current_separator_mode) parameter adjusts
 the behavior.

 An empty string at the end of the @rhombus(path) list corresponds to a
 URL that ends in a slash. For example, the result of
 @rhombus(URL.from_string("http://rhombus-lang.org/a/")) has a path field
 with strings @rhombus("a") and @rhombus(""), while the result of
 @rhombus(URL.from_string("http://rhombus-lang.org/a"))
 (string->url "http://racket-lang.org/a") has a path field with only the
 string @rhombus("a").

 When a @rhombus("file") URL is represented by a @rhombus(URL, ~class)
 instance, the path field is mostly a list of path elements. For Unix
 paths, the root directory is not included in @rhombus(path); its
 presence or absence is implicit in the @rhombus(is_path_absolute) flag.
 For Windows paths, the first element typically represents a drive, but a
 UNC path is represented by a first element that is @rhombus("") and then
 successive elements complete the drive components that are separated by
 @litchar{/} or @litchar{\}.

}

@doc(
  fun URL.from_string(s :: String) :: URL
  fun URL.from_path(p :: PathString || CrossPath) :: URL
  fun relative_path_to_relative_url_string(
    p :: (PathString.to_path || CrossPath) && CrossPath.Relative
  ) :: String
){

 The @rhombus(URL.from_string) and @rhombus(URL.from_string) functions
 convert from strings and paths to URLs.

 The @rhombus(relative_path_to_relative_url_string) converts a relative
 path into a string that represents a relative URL reference (e.g., using
 forward slashes, even on Windows).

}

@doc(
  method (u :: URL).to_string() :: String
  method (u :: URL).to_path(
    conv :: CrossPath.Convention = CrossPath.Convention.current()
  ) :: CrossPath
){

 Converts a @rhombus(URL, ~annot) to a string or path.

 The @rhombus(URL.to_string) conversion is used to @rhombus(print) a
 @rhombus(URL, ~class) object in @rhombus(#'text) mode, while
 @rhombus(#'expr) mode uses the default printing format for a class
 instance.

}

@doc(
  method (u :: URL).add(rel :: String) :: URL
){

 Adds a relative URL @rhombus(rel) to @rhombus(u) to produce a new URL.

}

@doc(
  Parameter.def current_encode_mode :: EncodeMode
  enum EncodeMode
  | recommended
  | unreserved
){


 The @rhombus(current_encode_mode) parameter determines how queries are
 encoded.

}

@doc(
  fun URL.from_handle(handle :: Any) :: URL
  method (u :: URL).to_handle() :: Any
){

 Converts between @rhombus(URL, ~class) objects and the URL
 representation used by Racket libraries.

}

@section(~tag: "form"){HTML Form Parsing}

@doc(
  fun form.urlencoded_encode(str :: String) :: String
  fun form.urlencoded_decode(str :: String) :: String
  fun form.list_to_urlencoded(kv :: List.of(KeyValue)) :: String
  fun form.urlencoded_to_list(s :: String) :: List.of(KeyValue)
  Parameter.def form.current_separator_mode
    :: AListSeparatorMode
  enum form.AListSeparatorMode
  | amp
  | semi
  | amp_or_semi
  | semi_or_amp

){

 Encodes and decodes according to @tt{application/x-www-form-urlencoded}
 rules from the HTML 4.0 specificiation. Encoding maps @litchar{!},
 @litchar{~}, @litchar{'}, @litchar{(}, and @litchar{)} using hex
 representations, which is the same choice as made by Java's
 @tt{URLEncoder}.

 The @rhombus(form.urlencoded_encode) and
 @rhombus(form.urlencoded_decode) functions work on individual strings,
 while @rhombus(form.list_to_urlencoded) and
 @rhombus(form.urlencoded_to_list) handle lists of key--value pairs.

 The @rhombus(form.current_separator_mode) parameter determines the
 separator used/recognized between associations in
 @rhombus(form.list_to_urlencoded), @rhombus(form.urlencoded_to_list),
 @rhombus(URL.from_string), and @rhombus(URL.to_string). The default
 value is @rhombus(#'amp_or_semi), which means that both @litchar{&} and
 @litchar{;} are treated as separators when parsing, and @litchar{&} is
 used as a separator when encoding. The @rhombus(#'semi_or_amp) mode is
 similar, but @litchar{;} is used when encoding. The other modes
 use/recognize only one of the separators.

}

@section(~tag: "uri"){URI Component Parsing}

@doc(
  fun uri.encode(str :: String) :: String
  fun uri.decode(str :: String) :: String
  fun uri.path_segment_encode(str :: String) :: String
  fun uri.path_segment_decode (str :: String) :: String
  fun uri.userinfo_encode(str :: String) :: String
  fun uri.userinfo_decode(str :: String) :: String
  fun uri.unreserved_encode(str :: String) :: String
  fun uri.unreserved_decode(str :: String) :: String
  fun uri.path_segment_unreserved_encode(str :: String) :: String
  fun uri.path_segment_unreserved_decode(str :: String) :: String
){

 Encoding and decoding functions specified by @url_rfc.
 The encoding, in line with @(rfc_name)'s recommendation, represents a
 character as-is, if possible. The decoding allows any characters to be
 represented by it hex value, and it allows a character to be incorrectly
 represented as-is. The ``unreserved'' encoders convert @litchar{!},
 @litchar{*}, @litchar{'}, @litchar{(}, and @litchar{)} to hex
 representations, which is not recommended by @rfc_name but avoids
 problems with some contexts.

}
