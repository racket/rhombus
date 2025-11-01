#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/network open)

@title(~tag: "udp"){UDP}

@doc(
  class UDP():
    implements Closeable
    constructor (
      ~family_host: family_host :: maybe(String) = #false,
      ~family_port: family_port :: maybe(PortNumber) = #false
    )
){

 A @rhombus(UDP, ~class) object represents a @deftech{UDP socket}.

 Since @rhombus(UDP, ~class) implements @rhombus(Closeable, ~class), the
 @rhombus(UDP) constructor can be used with @rhombus(Closeable.let).

}

@doc(
  method (udp :: UDP).close() :: Void
){

 Closes the UDP socket represented by @rhombus(udp). Closing an
 already-closed socket has no effect.

}


@doc(
  method (udp :: UDP).bind(
    ~host: host :: maybe(String) = #false,
    ~port: port :: ListenPortNumber = 0,
    ~reuse: reuse = #false
  ) :: Void
  method (udp :: UDP).is_bound() :: Boolean
){

 The @rhombus(UDP.bind) method binds a UDP socket to a local IP address
 and port. The @rhombus(host) and @rhombus(port) optionally constrain the
 binding. When not bound explicitly, a UDP socket is bound automatically
 by a send or receive operation.

 The @rhombus(UDP.is_bound) method reports whether @rhombus(udp) has
 been bound, independent of whether it was bound explicitly or
 automatically.

}

@doc(
  method (udp :: UDP).connect(
    ~host: host :: String,
    ~port: port :: PortNumber
  ) :: Void
  method (udp :: UDP).is_connected() :: Boolean
){

 The @rhombus(UDP.connect) method connects a UDP socket to a remote IP
 address and port. A UDP socket does not need to be connected to send
 data; use @rhombus(UDP.send_to) or @rhombus(UDP.send_to_evt) with an
 unconnected socket, and use @rhombus(UDP.send) or @rhombus(UDP.send_evt)

 The @rhombus(UDP.is_connected) method reports whether @rhombus(udp) has
 been connected.

}

@doc(
  method (udp :: UDP).receive(
    bytes :: MutableBytes,
    start_pos :: Nat = 0,
    end_pos :: Nat = bytes.length(),
    ~wait: wait :: Port.WaitMode && !matching(#'all) = #'some
  ) :: values(maybe(Nat), maybe(String), maybe(PortNumber))
){

 Receives data send to a UDP socket in a single packet. The data is
 written into @rhombus(bytes) starting at @rhombus(start_pos), and if
 more data than @rhombus(start_pos - end_pos) bytes are sent, the extra
 bytes are discarded.

 If @rhombus(wait) is @rhombus(#'some), then @rhombus(UDP.receive)
 blocks until data is received, and the results are never
 @rhombus(#false). If @rhombus(wait) is @rhombus(#'none), then
 @rhombus(UDP.receive) never blocks, and it immediately returns with
 @rhombus(#false) results if data is not immediately available. If
 @rhombus(wait) is @rhombus(#'enable_break), then @rhombus(UDP.receive)
 blocks the same as with @rhombus(#'some), but breaks are enabled while
 blocking; if breaks are disabled on entry, then either data is received
 or an @rhombus(Exn.Break) exception is thrown, but not both.

}

@doc(
  method (udp :: UDP).receive_evt(
    bytes :: MutableBytes,
    start_pos :: Nat = 0,
    end_pos :: Nat = bytes.length()
  ) :: Evt
  method (udp :: UDP).receive_ready_evt() :: Evt
){

 The @rhombus(UDP.receive_evt) method is similar to
 @rhombus(UDP.receive), but instead of receiving data, returns a
 @tech{synchronizable event} that is ready for synchronization when data
 is available to receive at @rhombus(udp). Synchronizing has the effect
 of receiving data, and the synchronization result is a
 @rhombus(PairList) of the three values (never @rhombus(#false)) that
 @rhombus(UDP.receive) would return.

 The @rhombus(UDP.receive_ready_evt) similarly returns a synchronizable
 event, but synchronization has no effect, and the synchronization result
 is the object itself.

}

@doc(
  method (udp :: UDP).send_to(
    ~host: host :: String,
    ~port: port :: PortNumber,
    bytes :: Bytes,
    start_pos :: Nat = 0,
    end_pos :: Nat = bytes.length(),
    ~wait: wait :: Port.WaitMode && !matching(#'some) = #'all
  ) :: Void || Boolean
){

 Sends data to the indicated remote IP address and port number using an
 unconnected UDP socket represented by @rhombus(udp). The data to be sent
 is the bytes from @rhombus(start_pos) (inclusive) to @rhombus(end_pos)
 (exclusive) in @rhombus(bytes).

 If @rhombus(wait) is @rhombus(#'all), then @rhombus(UDP.send_to) blocks
 until data can be sent (or, at least, queued at the operating-system
 level), and the result is @rhombus(#void). If @rhombus(wait) is
 @rhombus(#'none), then @rhombus(UDP.send_to) immediately returns without
 sending if data cannot be sent (or queued at the operating-system
 level), and the result is @rhombus(#false) in that case; otherwise, data
 is sent (or queued) immediately, and the result is @rhombus(#true). If
 @rhombus(wait) is @rhombus(#'enable_break), then @rhombus(UDP.send_to)
 blocks the same as with @rhombus(#'some), but breaks are enabled while
 blocking; if breaks are disabled on entry, then either data is sent
 or an @rhombus(Exn.Break) exception is thrown, but not both.

}

@doc(
  method (udp :: UDP).send(
    bytes :: Bytes,
    start_pos :: Nat = 0,
    end_pos :: Nat = bytes.length(),
    ~wait: wait :: Port.WaitMode && !matching(#'some) = #'all
  )  :: Void || Boolean
){

 Like @rhombus(UDP.send_to), but for a connected UDP socket (see
 @rhombus(UDP.connect)) where the destination IP address and port number
 were specified as the connection.

}

@doc(
  method (udp :: UDP).send_to_evt(
    ~host: host :: String,
    ~port: port :: PortNumber,
    bytes :: Bytes,
    start_pos :: Nat = 0,
    end_pos :: Nat = bytes.length()
  ) :: Evt
  method (udp :: UDP).send_evt(
    bytes :: Bytes,
    start_pos :: Nat = 0,
    end_pos :: Nat = bytes.length()
  ) :: Evt
  method (udp :: UDP).send_ready_evt() :: Evt

){

 The @rhombus(UDP.send_to_evt) and @rhombus(UDP.send_evt) methods are
 similar to @rhombus(UDP.send_to) or @rhombus(UDP.send), but instead of
 sending data, returns a @tech{synchronizable event} that is ready for
 synchronization when data is can be sent at @rhombus(udp).
 Synchroniziing has the effect of sending data, and the synchronization
 result is @rhombus(#void).

 The @rhombus(UDP.send_ready_evt) similarly returns a synchronizable
 event, but synchronization has no effect, and the synchronization result
 is the object itself.

}

@doc(
  method (udp :: UDP).addresses()
    :: values(String, ListenPortNumber, String, ListenPortNumber)
){

 Returns a UDP socket's local bound IP address and port number followed
 by the connection peer's IP address and port number. If @rhombus(udp) is
 unbound or unconnected, the corresponding results can be zeros.

}

@doc(
  property (udp :: UDP).ttl :: Byte
  property (udp :: UDP).ttl := (n :: Byte)
){

 A property for getting or setting a UDP socket's time-to-live value
 (i.e., the time-to-live value that it uses for sent packets).

}

@doc(
  method (udp :: UDP).multicast_join_group(
    ~address: addr :: String,
    ~host: host :: maybe(String)
  ) :: Void
  method (udp :: UDP).multicast_leave_group(
    ~address: addr :: String,
    ~host: host :: maybe(String)
  ) :: Void

  property (udp :: UDP).multicast_interface :: String
  property (udp :: UDP).multicast_interface := (intf :: maybe(String))

  property (udp :: UDP).multicast_loopback_on :: Boolean
  property (udp :: UDP).multicast_loopback_on := (on :: Any.to_boolean)

  property (udp :: UDP).multicast_ttl :: Byte
  property (udp :: UDP).multicast_ttl := (n :: Byte)
){

 Methods and properties to inspect or set the multicast configuration of
 a UDP socket.

}
