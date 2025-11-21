#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus open
      net/cookie
      net/cookie/user_agent
      net/url.URL
      net/http)

@(def cookie_eval: make_rhombus_eval())
@examples(
  ~eval:
    cookie_eval
  ~hidden:
    import:
      net/cookie/user_agent
      net/url open
)

@(def the_epoch:
    @elem{@tech(~doc: ModulePath'lib("rhombus/scribblings/reference/rhombus-reference.scrbl")'){the
  epoch} (midnight of January 1, 1970)})

@title(~tag: "user_agent"){Cookies for User Agents (Clients)}

@docmodule(net/cookie/user_agent)

The @rhombusmodname(net/cookie/user_agent) library provides cookie
support for user agents (i.e., clients).

@section{Cookie Objects}

@doc(
  class user_agent.Cookie(
    name :: cookie.Name,
    value :: cookie.Value,
    ~domain: domain :: cookie.Domain,
    ~path: path :: cookie.PathOrExtension,
    ~expiration_time: expiration_time :: PosInt,
    ~creation_time: creation_time :: PosInt,
    ~access_time: access_time :: PosInt,
    ~is_persistent: is_persistent :: Any.to_boolean,
    ~is_host_only: is_host_only :: Any.to_boolean,
    ~is_secure_only: is_secure_only :: Any.to_boolean,
    ~is_http_only: is_http_only :: Any.to_boolean
  )
){

 A structure representing a cookie from a user agent's point of view. A
 user angent normally will not need to construct a
 @rhombus(user_agent.Cookie, ~class) instance directly, except perhaps
 for testing. Instead, @rhombus(user_agent.parse.extract_cookies)
 produces instances for all the cookies received in a server's response.

 All times are represented as the number of seconds since @the_epoch,
 like the values produced by @rhombus(system.seconds).

}

@doc(
  method (c :: user_agent.Cookie).is_expired(
    now :: Int = system.seconds()
  ) :: Boolean
){

 Reports whether a cookie's experiation precedes @rhombus(now).

}

@doc(
  property (c :: user_agent.Cookie).handle
  fun user_agent.Cookie.from_handle(handle)
    :: user_agent.Cookie
){

 Returns the cookie's internal representation as recognized by Racket
 libraries or constructs a @rhombus(user_agent.Cookie, ~class) instance
 from such a representation.

}

@section{Cookie Jars}

@doc(
  class user_agent.CookieJar()
){

 A @rhombus(user_agent.CookieJar, ~class) instance is for saving cookies
 (imperatively) and extracting all cookies that match a given URL. An
 instance internally maintains a sorted order that mirrors the sort order
 specified by @RFC6265 for the @tt{Cookie} header.

}

@doc(
  method (cj :: user_agent.CookieJar).save_cookie(
    cookie :: user_agent.Cookie,
    ~via_http: via_http :: Any.to_boolean = #true
  ) :: Void
  method (cj :: user_agent.CookieJar).save_cookies(
    cookies :: Listable.to_list && List.of(user_agent.Cookie),
    ~via_http: via_http :: Any.to_boolean = #true
  ) :: Void
){

 Adds @rhombus(cookie) or all the cookies in @rhombus(cookies) to the
 jar @rhombus(cj), and also removes any expired cookies from
 @rhombus(cj).

 The @rhombus(via_http) argument should be true if the cookie was
 received via an HTTP API. If it is @rhombus(#false), the cookie will be
 ignored if the its ``HTTP only'' flag is set or if the cookie is
 attempting to replace an ``HTTP only'' cookie already present in the
 jar.

}

@doc(
  method (cj :: user_agent.CookieJar).extract_and_save_cookies(
    headers :: Map.of(String, Bytes) || (Listable.to_list && List.of(Bytes)),
    ~url: url :: URL,
    ~decode: decode :: Bytes -> String = Bytes.utf8_string
  ) :: Void
){

 Parses cookies out of @rhombus(headers) as received for @rhombus(url)
 and adds them via @rhombus(user_agent.CookieJar.save_cookies).

 The given @rhombus(headers) may be provided either as a map from string
 keys to bytes values, such as returned in a @rhombus(Response) from
 @rhombus(http.get), or as a list of unparsed header lines as byte
 strings. The @rhombus(decode) function is used just for values in the
 case that @rhombus(headers) is a map, and @rhombus(decode) is used for
 both keys and values in the case that @rhombus(headers) is a list of
 bytes.

@examples(
  ~eval:
    cookie_eval
  ~repl:
   def site = URL.from_string("http://test.example.com/apps/main")
   def example_cj = user_agent.CookieJar()
   example_cj.extract_and_save_cookies(
     { "X-Test-Header": #"isThisACookie=no",
       "Set-Cookie": #"a=b; Max-Age=2000; Path=/",
       "Set-Cookie": #"user=bob; Max-Age=86400; Path=/apps" },
     ~url: site
   )
   example_cj.cookie_header(site)
   example_cj.extract_and_save_cookies(
     [#"X-Ignore-This: thisIsStillNotACookie=yes",
      #"Set-Cookie: p=q; Max-Age=2000; Path=/",
      #"Set-Cookie: usersMom=alice; Max-Age=86400; Path=/apps"],
     ~url: site
   )
   example_cj.cookie_header(site)
)

}


@doc(
  method (cj :: user_agent.CookieJar).cookies_matching(
    url :: URL,
    ~is_secure :: is_secure = (URL.scheme(url) == "https")
  ) :: List.of(user_agent.Cookie)
){

 Returns all cookies in the jar @rhombus(jc) that should be sent in the
 @tt{Cookie} header for a request made to @rhombus(url). The
 @rhombus(is_secure) argument specifies whether the cookies will be sent
 via a secure protocol. (If not, cookies with the ``Secure'' flag set
 should not be returned by this method.)

 This method should produce its cookies in the order expected according
 to @RFC6265:

@itemlist(

  @item{Cookies with longer paths are listed before cookies with shorter paths.}

  @item{Among cookies that have equal-length path fields, cookies with
  earlier creation-times are listed before cookies with later
  creation-times.}

)

 If there are multiple cookies in the jar with the same name and
 different domains or paths, @RFC6265 does not specify which to send. The
 result listincludes all cookies that match the domain and path of the
 given URL, in the order specified above.

}

@doc(
  method (cj :: user_agent.CookieJar).cookie_header(
    url :: URL,
    ~encode: encode :: String -> Bytes = String.utf8_bytes,
    ~keep: keep :: user_agent.Cookie -> Any = fun (x): #true,
    ~skip: skip :: user_agent.Cookie -> Any = fun (x): #false
  ) :: maybe(Bytes)
){

 Finds any unexpired cookies matching @rhombus(url) in the jar
 @rhombus(cj), removes any for which @rhombus(keep) produces
 @rhombus(#false) or @rhombus(skip) produces a true value, and produces
 the value portion of a @tt{Cookie} HTTP request header. The result is
 @rhombus(#false) is no cookies match.

 Cookies with the ``Secure'' flag will be included in this header if and
 only if @rhombus(url.scheme) is @rhombus("https"), unless removed using
 the @rhombus(keep) or @rhombus(skip) function.

 See @rhombus(user_agent.CookieJar.extract_and_save_cookies) for an
 example.

}

@doc(
  Parameter.def user_agent.CookieJar.current
    :: user_agent.CookieJar
){

 A parameter for a default cookie jar.

}

@doc(
  property (cj :: user_agent.CookieJar).handle
){

 Returns the cookie jar's internal representation as recognized by
 Racket libraries.

}

@section{Parsing Utilities}

These parsing utilities are normally not used directly, but they are
used internally by @rhombus(user_agent.CookieJar.extract_and_save_cookies).

@doc(
  fun user_agent.parse.extract_cookies(
    headers :: Map.of(String, Bytes) || (Listable.to_list && List.of(Bytes)),
    ~url: url :: URL,
    ~decode: decode :: Bytes -> String = Bytes.utf8_string
  ) :: List.of(user_agent.Cookie)
){


 Parses @rhombus(headers) in the same way as
 @rhombus(user_agent.CookieJar.extract_and_save_cookies). but returns a
 list of cookies.

@examples(
  ~eval:
    cookie_eval
  ~repl:
   user_agent.parse.extract_cookies(
     [#"X-Ignore-This: thisIsStillNotACookie=yes",
      #"Set-Cookie: p=q; Max-Age=2000; Path=/",
      #"Set-Cookie: usersMom=alice; Max-Age=86400; Path=/apps"],
     ~url: site
   )
)

}

@doc(
  fun user_agent.parse.parse_cookie(
    bstr :: Bytes,
    ~url: url :: URL,
    ~decode: decode :: Bytes -> String = Bytes.utf8_string
  ) :: maybe(user_agent.Cookie)
){

 Parses a single cookie from a @tt{Set-Cookie} header value, returning a
 cookie if the header value is well-formed and @rhombus(#false)
 otherwise.

}

@doc(
  fun user_agent.parse.default_path(url :: URL)
    :: String
){

 Given a URL, produces the path that should be used for a cookie that has
 no @tt{Path} attribute, as specified in Section 5.1.4 of @RFC6265.

}

@doc(
  fun user_agent.parse.parse_date(str :: String)
    :: maybe(date.ZonedDateTime)
){

 Parses the given @rhombus(str) as a date, producing @rhombus(#false) if
 it is not possible to extract a date from the string using the algorithm
 specified in Section 5.1.1 of @RFC6265.

}

@doc(
  def user_agent.parse.min_cookie_seconds
  def user_agent.parse.max_cookie_seconds
){

 The largest and smallest integers that this user agent library will
 use, or be guaranteed to accept, as time measurements in seconds since
 @the_epoch.

}

@close_eval(cookie_eval)