#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url
      net/ssl)

@title(~tag: "session"){Sessions}

@doc( 
  class Session():
    constructor (
      ~pool_config: pool_config :: PoolConfig = PoolConfig(),
      ~ssl_context: context :: maybe(ssl.Context.Client) = ssl.Context.Client(),
      ~cookie_jar: cookie_jar :: maybe(CookieJar) = #false,
      ~proxies: proxies :: List.of(Proxy) = []
    )
  method (session :: Session).close() :: Void
){

 Creates a new session, which can house multiple requests with a common
 configuration.

 A session has a pool of connections, and the pool is configured through
 the @rhombus(pool_config) structor argument, which is a
 @rhombus(PoolConfig, ~class).

 The @rhombus(context) argument configures security properties for HTTP
 requests. By default, connections are secure using the operating
 system's default certificate store.

 A session is @rhombus(Closeable, ~class), where closing a session via
 the @rhombus(Session.close) method closes all of its associated connections and
 responses.

}

@doc( 
  method (session :: Session).request(
    uri :: Bytes || String || url.URL || LiteralURL,
    ~method: method :: Method = #'get,
    ~close: close :: Any.to_boolean = #false,
    ~stream: stream :: Any.to_boolean = #false,
    ~headers: headers :: Headers = {},
    ~params: params :: List.of(url.KeyValue) = [],
    ~auth: auth :: maybe(auth.Function) = #false,         
    ~data: data :: maybe(Bytes || String || Port.Input || payload.Function) = #false,
    ~timeouts: timeouts :: Timeouts = Timeouts(),
    ~max_attempts: max_attempts :: PosInt = 3,
    ~max_redirects: max_redirects :: PosInt = 16,
    ~user_agent: user_agent :: Bytes || String = current_user_agent()
  ) :: Response
){

 Sends an HTTP request. By default, a @tt{GET} request is sent, but the
 @rhombus(method) argument selects the type of request. The functions
 @rhombus(get), @rhombus(post), @rhombus(delete), @rhombus(head),
 @rhombus(options), @rhombus(patch), and @rhombus(put) are the same as
 calling @rhombus(Session.request) with the corresponding request type (on a
 @rhombus(Session, ~class) object that is given as a @rhombus(~session)
 argument, defaulting to a fresh session).

}

@doc(
  Parameter.def Session.current :: Session
){

 The default session used by @rhombus(get), @rhombus(post), etc.

}

@doc(
  enum Method:
    get
    post
    delete
    head
    options
    patch
    put
){

 Methods recognized by @rhombus(Session.request).

}


@doc(
  annot.macro 'Headers'
){

 A shorthand for @rhombus(Map.of(String, Bytes || String)).

}
