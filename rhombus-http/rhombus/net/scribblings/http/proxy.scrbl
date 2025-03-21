#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "proxy"){Proxy Configuration}


@doc( 
  class Proxy():
    constructor (
      ~matches: matches :: url.URL -> Boolean,
      ~connect: connect :: (HTTPConn, url.URL, maybe(SSLContext)) -> Void
    )
    
  fun Proxy.http(
    ~proxy_url: proxy_url :: String || Bytes || url.URL,
    ~matches: matches :: url.URL -> Boolean:
                fun (u :: url.URL): u.scheme == "http"
  ) :: Proxy
  
  fun Proxy.https(
    ~proxy_url: proxy_url :: String || Bytes || url.URL,
    ~matches: matches :: url.URL -> Boolean:
                = fun (u :: url.URL): u.scheme == "https"
  ) :: Proxy
){

 A @rhombus(Proxy, ~class) represents a configuration for constructing a
 @rhombus(Session) that causes some connections to be created through the
 proxy. In general, a @rhombus(Proxy, ~class) has a @rhombus(matches)
 function to determine whether a request address should use the proxy,
 and a @rhombus(connect) function that creates the proxied connection.

 The @rhombus(Proxy.http) configuration specifies an HTTP @tt{CONNECT}
 proxy at the given @rhombus(proxy_url) and optionally with a
 @rhombus(matches) function.

 The @rhombus(Proxy.https) configuration is similar to
 @rhombus(Proxy.http), but using HTTPS.

}
