#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "response"){Responses}

@doc(
  ~nonterminal:
    val_bind: def bind ~defn
    status_code_bind: def bind ~defn
    bstr_bind: def bind ~defn
    bstr_list_bind: def bind ~defn
    response_list_bind: def bind ~defn
  
  class Response():
    expression ~none
    binding 'Response($field, ...)'
  
  grammar field:
    ~status_line: $bstr_bind
    ~status_code: $status_code_bind
    ~status_message: $bstr_bind
    ~http_version: $bstr_bind
    ~history: $response_list_bind
    ~headers: { $key_str: $bstr_bind, ... }
    ~headers: { $key_str: $bstr_bind, ..., & $bstr_list_bind }
    ~body: $bstr_bind

){

 A @rhombus(Response, ~class) represents a request sent via
 @rhombus(Session.request) or through one of the shorthand functions like
 @rhombus(get).

 A @rhombus(Response, ~class) may still be in the process of receiving
 its data when it is returned, so methods like @rhombus(Response.body)
 may block while data is read. A @rhombus(Response, ~class) is
 @rhombus(Closeable, ~class) via @rhombus(Response.close).

 As a binding form, @rhombus(Response, ~bind) extracts and sometimes
 converts response information, blocking as needed until the information
 is available. The extracted information is matched against a given
 binding.

@itemlist(

 @item{@rhombus(~status_line): Matches the same byte string result as
  produced by @rhombus(Response.status_line).}

 @item{@rhombus(~status_code): Matches the same integer result as
  produced by @rhombus(Response.status_code).}

 @item{@rhombus(~status_message): Matches the same byte string result as
  produced by @rhombus(Response.status_message).}

 @item{@rhombus(~http_version): Matches the same byte string result as
  produced by @rhombus(Response.http_version).}

 @item{@rhombus(~history): Matches the list result as
  produced by @rhombus(Response.history).}

 @item{@rhombus(~headers): Matches against indivdual fields in a
  response header. These fields are located in the byte strings returned
  by @rhombus(Response.raw_headers), where @rhombus(key_str)s are matched
  case-insensitively to field name, and each @rhombus(bstr_bind) after a
  @rhombus(key_str) is matched against the byte-string value of that
  field. If @rhombus(& bstr_list_bind) is present, it is matched against
  the list of byte strings from @rhombus(Response.raw_headers), but with
  lines matched to @rhombus(key_str)s removed.}

 @item{@rhombus(~body): Matches the byte string result as
  produced by @rhombus(Response.body).}

)

}

@doc(
  property (resp :: Response).status_line :: Bytes
  property (resp :: Response).status_code :: StatusCode
  property (resp :: Response).status_message :: Bytes
  property (resp :: Response).http_version :: Bytes
  property (resp :: Response).raw_headers :: List.of(Bytes)
){

 Accesses the raw immediate data for a request response.

}

@doc(
  method (resp :: Response).headers() :: Map.of(String, Bytes)
){

 Returns the same result as @rhombus(Response.raw_headers), but parsed
 into a map from field names to values.

}

@doc(
  method (resp :: Response).output() :: Port.Input
  method (resp :: Response).body() :: Bytes
){

 The @rhombus(Response.output) method returns an output port that
 provides the response's body content. The @rhombus(Response.body) method
 reads and records all data from that output port; calling
 @rhombus(Response.body) a second time returns the same recorded data.

 If @rhombus(Response.output) is called after @rhombus(Response.body),
 then the returned output port will be closed. If
 @rhombus(~stream: #false) or @rhombus(~close: #true) were provided to
 @rhombus(Session.request) to obtain the @rhombus(Response, ~class)
 object, then @rhombus(Response.body) is effectively called already.

}

@doc(
  method (resp :: Response).history() :: List.of(Response)
){

 Reports redirections taken to arrive at the final response. The
 redirection responses are in reverse order in the result list (i.e.,
 most recent first).

}

@doc(
  method (resp :: Response).close() :: Void
  method (resp :: Response).drain() :: Void
){

 The @rhombus(Response.close) method terminates any communication still
 in process to reeceive the result.

 The @rhombus(Response.drain) method reads all data for the response
 body and records it. A @rhombus(Response.drain) call has no effect if
 the content is already read.

}

@doc(
  annot.macro 'StatusCode'
){

 Equivalent to @rhombus(Int.in(100, 999)).

}

@doc(
  Parameter.def current_user_agent :: String
){

 Supplies a default value for the @rhombus(~user_agent) argument of
 @rhombus(Session.request), which determines the @tt{User-Agent} field of
 the request.

}

@doc(
  property (resp :: Response).handle
){

 Returns a Racket representation of the response.

}
