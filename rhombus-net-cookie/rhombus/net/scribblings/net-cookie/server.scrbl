#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus open
      rhombus/date
      net/cookie
      net/cookie/server)

@(def cookie_eval: make_rhombus_eval())
@examples(
  ~eval:
    cookie_eval
  ~hidden:
    import net/cookie/server
)

@title(~tag: "server"){Cookies for Servers}

@docmodule(net/cookie/server)

The @rhombusmodname(net/cookie/server) library provides cookie support
for servers, which need to create cookies and encode them for response
headers.

@doc(
  class server.Cookie(
    name :: cookie.Name,
    value :: cookie.Value,
    ~exp_date: exp_date :: maybe(date.ZonedDateTime) = #false,
    ~max_age: max_age :: maybe(PosInt) = #false,
    ~domain: domain :: maybe(cookie.Domain) = #false,
    ~path: path :: maybe(cookie.PathOrExtension) = #false,
    ~extension: extension :: maybe(cookie.PathOrExtension) = #false,
    ~is_secure: is_secure :: Boolean = #false,
    ~is_http_only: is_http_only :: Boolean = #false
  )
){

 Constructs a cookie for sending to a user agent (i.e., client). The
 @rhombus(#'text)-mode printed form of the cookie, which can be obtained
 via @rhombus(to_string), is a values for a @tt{Set-Cookie} HTTP response
 header suitable for sending the cookie to a user agent.

@examples(
  ~eval:
    cookie_eval
  ~repl:
    to_string(server.Cookie("rememberUser", "bob", ~path: "/main"))
)

 Both @rhombus(exp_date) and @rhombus(max_age) are for specifying a time
 at which the user agent should remove the cookie from its cookie store.
 The @rhombus(exp_date) argument is for specifying this expiration time
 as a date, while @rhombus(max_age) is for specifying it as a number of
 seconds in the future. If both @rhombus(exp_date) and @rhombus(max_age)
 are given as non-@rhombus(#false), an @(RFC6265)-compliant user agent will
 disregard the @rhombus(exp_date) and use the @rhombus(max_age).

 A non-@rhombus(#false) @rhombus(domain) argument indicates that the
 recipient should send the cookie back to the server only if the hostname
 in the request URI is either @rhombus(domain) itself or a host within
 @rhombus(domain).

 A non-@rhombus(#false) @rhombus(path) argument indicates that the
 recipient should send the cookie back to the server only if
 @rhombus(path) is a prefix of the request URI's path.

 When @rhombus(is_secure) is @rhombus(#true), a flag tells the recipient
 that the cookie may only be sent if the request URI’s scheme specifies a
 ``secure'' protocol (presumably HTTPS).

 When @rhombus(is_http_only) is @rhombus(#true), a flag tells the
 recipient that the cookie may be communicated only to a server and only
 via HTTP or HTTPS.

 @bold{The flag @rhombus(is_http_only) and @rhombus(is_secure) flags are
  important for security.} Browsers provide JavaScript access to cookies
 (for example, via @tt{document.cookie}), and consequently, when cookies
 contain sensitive data such as user session info, malicious JavaScript
 can compromise that data. The @tt{HttpOnly} cookie flag, set by
 @rhombus(is_http_only) argument, instructs the browser not to make this
 cookie available to JavaScript code. If a cookie is intended to be
 confidential, both @rhombus(is_secure) and @rhombus(is_http_only) should
 be @rhombus(#true), and all connections should use HTTPS. Some older
 browsers do not support this flag; see the
 @hyperlink("https://owasp.org/"){OWASP page} on @tt{HttpOnly} for more
 info.

}


@doc(
  fun server.Cookie.clear_string(
    name :: cookie.Name,
    ~domain: domain :: maybe(cookie.Domain) = #false,
    ~path: path :: maybe(cookie.PathOrExtension) = #false
  ) :: String
){

 Produces a string containing a @tt{Set-Cookie} header value suitable
 for telling a user agent to clear the cookie with the given
 @rhombus(name). This is done, as per @RFC6265, by sending a cookie with
 an expiration date in the past.

@examples(
  ~eval:
    cookie_eval
  ~repl:
    server.Cookie.clear_string("rememberUser", ~path: "/main")
)

}

@doc(
  fun server.Cookie.header_to_map(
    header :: Bytes,
  ) :: Map.of(Bytes, Bytes)

  fun server.Cookie.header_to_map(
    header :~ Bytes,
    ~decode: decode :: (Bytes -> Any)
  ) :: Map
){


 Given the value part of a @tt{Cookie} header, produces a map of all
 cookie-name-to-value mappings in the header. If a @rhombus(decode)
 function is given, it is applied to each key and each value before
 inserting a map. If a key in the header has no value, then
 @rhombus(#""), or @rhombus(decode(#"")) is used as the value. Invalid
 cookies will not be present in the result.

@examples(
  ~eval:
    cookie_eval
  ~repl:
    server.Cookie.header_to_map(#"SID=31d4d96e407aad42; lang=en-US")
    server.Cookie.header_to_map(#"SID=31d4d96e407aad42; lang=en-US",
                                ~decode: Bytes.utf8_string)
    server.Cookie.header_to_map(
      #"seenIntro=; logins=3",
      ~decode: (fun (s): String.maybe_to_number(s) || s)
                 ∘ Bytes.utf8_string
    )
)

}

@doc(
  property (c :: server.Cookie).handle
){

 Returns the cookie's internal representation as recognized by Racket
 libraries.

}

@close_eval(cookie_eval)
