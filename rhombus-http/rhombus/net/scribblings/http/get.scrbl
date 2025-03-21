#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "get"){Sending Requests}

@doc( 
  fun get(
    uri :: Bytes || String || url.URL || LiteralURL,
    ~session: session :: Session = Session.current(),
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
  fun post(....) :: Response
  fun delete(....) :: Response
  fun head(....) :: Response
  fun options(....) :: Response
  fun patch(....) :: Response
  fun put(....) :: Response
){

 Sends a request using the @rhombus(session) argument's
 @rhombus(Session.request) method. All of these functions accept the same
 arguments as @rhombus(get), and each corresponds to calling
 @rhombus(Session.request) with a corresponding @rhombus(~method)
 argument and passing along all the other arguments as given.

 See @rhombus(Session.request) for descriptions of the arguments other
 than @rhombus(session).

}
