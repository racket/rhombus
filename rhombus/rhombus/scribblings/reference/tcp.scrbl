#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/network open)

@title(~tag: "tcp"){TCP}

@doc(
  annot.macro 'TCP.Port'
  fun TCP.addresses(port :: TCP.Port)
    :: values(String, PortNumber, String, ListenPortNumber)
  fun TCP.abandon(port :: TCP.Port) :: Void
){

 A TCP connection is represented by a part of @tech{ports}, and an
 @tech{input port} and an @tech{output port}. Both of the ports are
 @rhombus(TCP.Port, ~annot)s, so they can be used with functions like
 @rhombus(Port.addresses) and @rhombus(Port.abandon).

 The @rhombus(TCP.addresses) function returns a connection's local IP
 address and port number followed by the connection peer's IP address and
 port number.

 The @rhombus(TCP.abandon) function is almost the same as closing a port
 with @rhombus(Port.Input.close) or @rhombus(Port.Output.close), but in
 in the case of closing an output port, the connection peer does not
 receive an end-of-file in its input stream.

}

@doc(
  fun TCP.connect(
    ~host: host :: String,
    ~port: port :: PortNumber,
    ~local_host: local_host :: maybe(String) = #false,
    ~local_port: local_port :: maybe(PortNumber) = #false,
    ~wait: wait :: NetworkWait = #'all
  ) :: values(Port.Input && Port.FileStream && TCP.Port,
              Port.Output && Port.FileStream && TCP.Port)
){

 Connects as a client to a TCP server at @rhombus(host) and
 @rhombus(port).

 If @rhombus(local_host) or @rhombus(local_port) is not
 @rhombus(#false), it can determine the interface (at the
 operating-system level) on the local machine that is used for the TCP
 connection.

 If @rhombus(wait) is @rhombus(#'enable_break) and breaks are currently
 disabled, then breaks are enabled while waiting for the connection to be
 created. In that case, either an @rhombus(Exn.Break) exception is thrown
 or a connection and ports are created, but not both.

 Since @rhombus(TCP.connect, ~class) returns two ports, it can be used
 with @rhombus(Closeable.let).


}

@doc(
  class TCPListener():
    implements Closeable
    constructor (
      ~host: host :: maybe(String) = #false,
      ~port: port :: ListenPortNumber = 0,
      ~reuse: reuse = #false,
      ~max_allow_wait: max_allow_wait :: NonnegInt = 4
    )

  fun TCP.listen(
      ~host: host :: maybe(String) = #false,
      ~port: port :: ListenPortNumber = 0,
      ~reuse: reuse = #false,
      ~max_allow_wait: max_allow_wait :: NonnegInt = 4
  ) :: TCPListener
){

 A @rhombus(TCPListener, ~class) represents a TCP listener for a server
 that can accept connections via @rhombus(TCPListener.accept).

 If @rhombus(host) is not @rhombus(#false), it determines the IP
 addresses and interfaces that accept connections for the listener. The
 @rhombus(reuse) and @rhombus(max_allow_wait) arguments similarly
 configure the created listener.

 The @rhombus(TCP.listen) function accepts the same arguments as the
 @rhombus(TCPListener, ~class) constructor and returns a listener created
 with those arguments.

 Since @rhombus(TCPListener, ~class) implements
 @rhombus(Closeable, ~class), the @rhombus(TCPListener) constructor or
 @rhombus(TCP.listen) function can be used with @rhombus(Closeable.let).

}

@doc(
  method (lnr :: TCPListener).accept(
    ~wait: wait :: NetworkWait = #'all
  ) :: values(Port.Input && Port.FileStream && TCP.Port,
              Port.Output && Port.FileStream && TCP.Port)
  method (lnr :: TCPListener).accept_ready() :: Boolean
  method (lnr :: TCPListener).accept_evt() :: Evt
){

 The @rhombus(TCPListener.accept) method accepts a connection to a
 listener, blocking until a connection is available.

 The @rhombus(TCPListener.accept_ready) method reports whether a
 @rhombus(TCPListener.accept) call will complete immediately because a
 connection is available.

 The @rhombus(TCPListener.accept_evt) method returns a
 @tech{synchronizable event} that is ready for synchronization when a
 connection is available. The synchronization result is a
 @rhombus(PairList) of containing two values, which are ports like the
 two results of @rhombus(TCPListener.accept).

}

@doc(
  method (lnr :: TCPListener).addresses()
    :: values(String, PortNumber)
){

 Reports the local IP address and port number of @rhombus(lnr).

}

@doc(
  method (lnr :: TCPListener).close()
){

 Closes the listener. Previously accepted connections are unaffected by
 closing a listener.

}
