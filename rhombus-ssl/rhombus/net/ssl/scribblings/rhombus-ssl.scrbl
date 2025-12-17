#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      rhombus/network
      rhombus/collect
      net/ssl)

@(def ref_doc: ModulePath'lib("rhombus/scribblings/reference/rhombus-reference.scrbl")')

@(def ALPN:
    @hyperlink("https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation"){ALPN})
@(def RFC2818:    
    @hyperlink("http://www.ietf.org/rfc/rfc2818.txt"){RFC 2818})

@(def envvar = tt)

@title(~category: #'#{net-library}){SSL: Secure Communication}

@docmodule(net/ssl)

The @rhombusmodname(net/ssl) library supports secure network communication
via the OpenSSL API and libraries that are included with Racket or
provided by the operating system.


@doc(
  def ssl.is_available :: Boolean
  def ssl.unavailable_reason :: maybe(String)
){

 The @rhombus(ssl.is_available) variable reports whether SSL
 functionality is available, and if not, @rhombus(ssl.unavailable_reason)
 provides an explanation of why.

}

@table_of_contents()

@// ------------------------------------------------------------
@section(~tag: "port"){Secure Ports}

@doc(
  annot.macro 'ssl.Port'
  annot.macro 'ssl.Port.Input'
  annot.macro 'ssl.Port.Output'
){

 Satisfied by an input port or output that represents an SSL connection.
 An SSL port also satisifies @rhombus(Port, ~annot), and it also
 satisfies either @rhombus(Port.Input, ~annot) or
 @rhombus(Port.Output, ~annot).

 A pair of @rhombus(ssl.Port, ~annot)s is typically created by
 @rhombus(ssl.connect) or @rhombus(ssl.Listener.accept), but
 @rhombus(ssl.Port, ~annot)s can also be created as a layer on existing
 ports via @rhombus(ssl.Port.from_ports).

}

@doc(
  fun ssl.Port.from_ports(
    in :: Port.Input,
    out :: Port.Output,
    ~mode: mode :: ssl.Port.Mode = #'connect,
    ~context: context :: ssl.Context = (if mode == #'connect
                                        | ssl.Context.Client()
                                        | ssl.Context.Server()),
    ~host: host :: maybe(String) = #false,
    ~alpn_protocols: alpn_protocols :: List.of(Bytes) = [],
    ~close_original: close_original = #false,
    ~shutdown_on_close: shutdown_on_close = #false,
    ~exn: exn :: (String, Continuation.Marks) -> Any = Exn.Fail
  ) :~ values(ssl.Port.Input, ssl.Port.Output)

  enum ssl.Port.Mode
  | connect
  | accept
){

 Wraps existing ports to use the SSL protocol. The @rhombus(ssl.connect)
 and @rhombus(ssl.Listener.accept) functions are convenience wrappers on
 @rhombus(network.TCP.connect) and @rhombus(network.TCPListener.accept),
 respectively, plus @rhombus(ssl.Port.from_ports).

 The @rhombus(mode) argument determines whether the given @rhombus(in)
 and @rhombus(out) are wrapped to use the client (@rhombus(#'connect)) or
 server (@rhombus(#'accept)) half of the protocol.

 The @rhombus(context) argument configures properties of the connection,
 and the kind of context must be consistent with @rhombus(mode). See
 @rhombus(ssl.Context.Client) and @rhombus(ssl.Context.Server) for more
 information.

 If hostname verification is enabled (see
 @rhombus(ssl.Context.set_verify_hostname)), the peer’s certificate is
 checked against @rhombus(host).

 A non-empty @rhombus(alpn_protocols) argument is used in
 @rhombus(#'connect) mode, in which case the client attempts to use
 @ALPN; see also @rhombus(ssl.connect) and
 @rhombus(ssl.Port.selected_alpn). If @rhombus(mode) is
 @rhombus(#'accept), then @rhombus(alpn_protocols) must be empty; use
 @rhombus(ssl.Context.Server.set_server_alpn) to set the ALPN protocols for a
 server context.

 If @rhombus(close_original) is a true value, then @rhombus(in) and
 @rhombus(out) are closed when both of the returned ports are closed.

 If @rhombus(shutdown_on_close) is a true value, then when @rhombus(out)
 is closed before @rhombus(in) is closed, then a shutdown message is sent
 to the connection peer. Otherwise, an early close of @rhombus(out) is
 not reported to the connection peer.

 If an error is encountered during the protocol initialization, then
 @rhombus(exn) is used to construct the exception that is raised.
 Supplying @rhombus(Exn.Fail.Network) as @rhombus(exn) might be useful,
 for example.

}


@doc(
  method (port :: ssl.Port).set_verify(
    mode :: ssl.Context.VerifyMode = #'always
  )
){

  Like @rhombus(ssl.Context.set_verify) on the context used to create
  @rhombus(port), but applicable after the connection is created to
  renegotiate the connection.

}

@doc(
  method (port :: ssl.Port).is_peer_verified() :: Boolean
){

 Returns @rhombus(#true) if the peer of SSL port @rhombus(port) has
 presented a valid and verified certificate, @rhombus(#false) otherwise.

}

@doc(
  method (port :: ssl.Port).peer_certificate_hostnames()
    :: List.of(String)
){

  Reports the list of hostnames for which the certificate of
  @rhombus(port)'s peer is valid according to @RFC2818. If the peer has
  not presented a certificate, @rhombus([]) is returned.

  The result list may contain both hostnames such as
  @rhombus("www.racket-lang.org") and hostname patterns such as
  @rhombus("*.racket-lang.org").

}

@doc(
  method (port :: ssl.Port).peer_check_hostname(host :: String) :: Boolean
){

 Returns @rhombus(#true) if the peer certificate of @rhombus(port) is valid for
 hostname according to @RFC2818, @rhombus(#false) otherwise.

}

@doc(
  method (port :: ssl.Port).peer_subject_name() :: maybe(Bytes)
  method (port :: ssl.Port).peer_issuer_name() :: maybe(Bytes)
){

 If @rhombus(ssl.Port.is_peer_verified) would return @rhombus(#true) for
 @rhombus(port), the result is a byte string for the subject or issuer
 field of the certificate presented by the SSL port’s peer, otherwise the
 result is @rhombus(#false).

 Use @rhombus(ssl.Port.peer_certificate_hostname) or
 @rhombus(ssl.Port.peer_certificate_hostnames) instead to check the
 validity of an SSL connection.

}

@doc(
  method (port :: ssl.Port).selected_alpn() :~ maybe(Bytes)

){

 Returns the @ALPN protocol selected during negotiation, or
 @rhombus(#false) if no protocol was selected.

 If a server does not support any of the protocols proposed by a client,
 it might reject the connection or it might accept the connection without
 selecting an application protocol. Always check the selected protocol
 after making a client connection.

}

@doc(
  method (port :: ssl.Port).addresses()
    :: values(String, network.PortNumber, String, network.ListenPortNumber)
  method (port :: ssl.Port).abandon()
){

 Like @rhombus(network.TCP.addresses) and @rhombus(network.TCP.abandon).

}


@// ------------------------------------------------------------
@section(~tag: "client"){Secure Clients}

@doc(
  fun ssl.connect(
    ~host: host :: String,
    ~port: port :: network.PortNumber,
    ~context: context :: ssl.Context.Client = ssl.Context.Client(),
    ~alpn_protcols: alpn_protocols :: List.of(Bytes) = []
  ) :~ values(ssl.Port.Input, ss.Port.Output)
){

 Connects to @rhombus(host) at @rhombus(port), similiar to (and building
 on) @rhombus(network.TCP.connect), but for a secure connection. The
 @rhombus(context) argument along with @rhombus(alpn_protocols)
 configures the connection. By default, @rhombus(context) is a secure
 connection that checks a certificate that @rhombus(host) provides as
 part of the SSL protocol.

 If @rhombus(alpn_protocols) is not empty, the client attempts to use
 @ALPN to negotiate the connection protocol. Protocols should be listed
 in order of preference, and each protocol must be a byte string with a
 length between 1 and 255 (inclusive). See also
 @rhombus(ssl.Port.selected_alpn).

 Closing the resulting output port does not send a shutdown message to
 the server. See also @rhombus(ssl.Port.from_ports) and its
 @rhombus(~shutdown_on_close) argument.

}

@// ------------------------------------------------------------
@section(~tag: "server"){Secure Servers}

@doc(
  class ssl.Listener():
    constructor (
      ~host: host :: maybe(String) = #false,
      ~port: port :: network.ListenPortNumber,
      ~context: context :: ssl.Context.Server = ssl.Context.Server(),
      ~reuse: reuse :: Any = #false,
      ~max_allow_wait: max_allow_wait :: Nat = 5,
    ) :~ ssl.Listener
){

 Implements an SSL server through an underlying TCP listener.
 The server is configured via @rhombus(context), while the
 @rhombus(reuse) and @rhombus(max_allow_wait) arguments are as for
 @rhombus(network.TCP.listen).

 Call @rhombus(TCPListener.load_certificate_chain) and
 @rhombus(TCPListener.load_private_key) to avoid a ``no shared cipher''
 error on accepting connections. The file whose path is
 @rhombus(collect.file_path(~collect: "openssl", ~file: "test.pem")) is a
 suitable argument for both calls when testing. Since @filepath{test.pem}
 is public, however, such a test configuration obviously provides no
 security.

 An SSL listener is a @tech(~doc: ref_doc){synchronizable event}. It is
 ready---with itself as its value---when the underlying TCP listener is
 ready. At that point, however, accepting a connection with
 @rhombus(ssl.Listener.accept) may not complete immediately, because
 further communication is needed to establish the connection.

 An SSL listener implements @rhombus(Closeable, ~class), so it can be
 used with @rhombus(Closeable.let).

}

@doc(
  fun ssl.listen(
    ~host: host :: maybe(String) = #false,
    ~port: port :: network.ListenPortNumber,
    ~context: context :: ssl.Context.Server = ssl.Context.Server(),
    ~reuse: reuse :: Any = #false,
    ~max_allow_wait: max_allow_wait :: Nat = 5,
  ) :~ ssl.Listener
){

  Analogous to @rhombus(network.TCP.listen), equivalent to constructing
  @rhombus(ssl.Listener).

}

@doc(
  method (lnr :: ssl.Listener).close()
){

 Closes an SSL listener. Closing the listener means that no new
 connections can be accepted, but existing connections can continue.

}

@doc(
  method (lnr :: ssl.Listener).accept(
    ~wait: wait :: network.NetworkWait = #'all
  ) :: values(ssl.Port.Input, ssl.Port.Output)
){

 Analogous to @rhombus(network.TCPListener.accept), accepts an SSL
 client connection for a server's listener.

}

@doc(
  method (lnr :: ssl.Listener).addresses()
    :: values(String, network.PortNumber)
){

 Like @rhombus(network.TCPListener.addresses).

}

@doc(
  method (lnr :: ssl.Listener).load_certificate_chain(
    path :: PathString
  )

  method (lnr :: ssl.Listener)
    .load_suggested_certificate_authorities(path :: PathString)

  method (lnr :: ssl.Listener).load_private_key(
    key :: PathString || ssl.Context.PrivateKey,
    ~kind: kind :: ssl.Context.KeyKind = #'rsa
  )
){

 These methods are like @rhombus(ssl.Context.load_certificate_chain),
 @rhombus(ssl.Context.load_suggested_certificate_authorities), and
 @rhombus(ssl.Context.load_private_key), but for an already-created SSL
 listener.

}

@// ------------------------------------------------------------
@section(~tag: "context"){Secure Contexts}

@doc(
  annot.macro 'ssl.Context'
  annot.macro 'ssl.Context.Client'
  annot.macro 'ssl.Context.Server'

  fun ssl.Context.Client(~secure: secure = #true)
    :: ssl.Context.Client

  fun ssl.Context.Server(
    ~private_key: private_key :: maybe(ssl.Context.PrivateKey) = #false,
    ~certificate_chain: certificate_chain :: maybe(Path) = #false
  ) :: ssl.Context.Server  
){

 A @rhombus(ssl.Context, ~annot) represents a security configuration for
 an SSL connection. A @rhombus(ssl.Context.Client, ~annot) context must
 be used for a client connection, while a
 @rhombus(ssl.Context.Client, ~annot) context is for a server listener.

 If @rhombus(secure) is true for @rhombus(ssl.Context.Client), the
 resulting context is sealed in the sense of @rhombus(ssl.Context.seal),
 and it is configured for peer certificate and hostname verification.

 Non-@rhombus(#false) arguments passed to @rhombus(ssl.Context.Server)
 correspond to calling @rhombus(ssl.Context.load_private_key) and
 @rhombus(ssl.Context.load_certificate_chain) on the created context,
 respectively.

}

@doc(
  method (ctx :: ssl.Context).load_verify_source(
    src :: PathString || ssl.Context.VerifySource
  )

  annot.macro 'ssl.Context.VerifySource'
  
  fun ssl.Context.VerifySource.default()
    :: ssl.Context.VerifySource
  fun ssl.Context.VerifySource.file(path :: PathString)
    :: ssl.Context.VerifySource
  fun ssl.Context.VerifySource.directory(path :: PathString)
    :: ssl.Context.VerifySource
  fun ssl.Context.VerifySource.win_store(bstr :: Bytes)
    :: ssl.Context.VerifySource
  fun ssl.Context.VerifySource.mac_keychain(path :: maybe(PathString))
    :: ssl.Context.VerifySource

  property (vs :: ssl.Context.VerifySource).handle  
){

 The @rhombus(ssl.Context.load_verify_source) method loads verification
 sources from @rhombus(src) into context. Currently, only certificates
 are loaded; the certificates are used to verify the certificates of a
 connection peer. Call this procedure multiple times to load multiple
 sets of trusted certificates.

 The following kinds of verification sources are supported as @rhombus(src):

@itemlist(

  @item{A @rhombus(PathString, ~annot) is equivalent to
   @rhombus(ssl.Context.VerifySource.file(src)).}

  @item{A @rhombus(ssl.Context.VerifySource.default) source loads sources,
   depending on the platform:

   @itemlist(

    @item{On Linux, the default sources are determined by the
    @envvar{SSL_CERT_FILE} and @envvar{SSL_CERT_DIR} environment variables,
    if the variables are set, or the system-wide default locations
    otherwise.}

    @item{On Windows, the default sources consist of the system
    certificate store for root certificates, the same as
    @rhombus(ssl.Context.VerifySource.win_store("ROOT")).}

    @item{On Mac OS, the default sources consist of the OS trust
    anchor (root) certificates, the same as
    @rhombus(ssl.Context.VerifySource.mac_keychain(#false)).}

  )}

  @item{A @rhombus(ssl.Context.VerifySource.file) source is treated as a
   PEM file containing root certificates. The file is loaded immediately.}

  @item{A @rhombus(ssl.Context.VerifySource.directory) source should
   contain PEM files with hashed symbolic links (see the @exec{openssl
    c_rehash} utility). The directory contents are not loaded immediately;
   rather, they are searched only when a certificate needs verification.}

  @item{A @rhombus(ssl.Context.VerifySource.win_store) source loads
   certificates from the name store immediately. Only supported on
   Windows.}

  @item{A @rhombus(ssl.Context.VerifySource.mac_keychain) source loads
   certificates from the Mac OS keychain stored at the insicated path, or
   it load from trust anchor (root) certificates (as returned by
   @tt{SecTrustCopyAnchorCertificates}) if @rhombus(#false) is supplied
   instead of a path. Only supported on Mac OS.}

)

 You can use the file @filepath{test.pem} as report by
 @rhombus(collect.file_path(~collect: "openssl", ~file: "test.pem")) for
 testing purposes. Since @filepath{test.pem} is public, such a test
 configuration obviously provides no security.

}

@doc(
  method (ctx :: ssl.Context).load_private_key(
    key :: PathString || ssl.Context.PrivateKey,
    ~kind: kind :: ssl.Context.KeyKind = #'rsa
  )

  annot.macro 'ssl.Context.PrivateKey'
  
  fun ssl.Context.PrivateKey.pem(path :: PathString)
    :: ssl.Context.PrivateKey
  fun ssl.Context.PrivateKey.pem_bytes(bstr :: Bytes)
    :: ssl.Context.PrivateKey
  fun ssl.Context.PrivateKey.der(path :: PathString)
    :: ssl.Context.PrivateKey

  property (vs :: ssl.Context.PrivateKey).handle  
  
  enum ssl.Context.KeyKind
  | rsa
  | any
){

 Loads the first private key from @rhombus(key) for the given context.
 The key goes with the certificate that identifies the client or server.
 Like @rhombus(ssl.Context.load_certificate_chain), this method is
 usually used with server contexts or listeners and seldom with client
 contexts.

 If @rhombus(kind) is @rhombus(#'rsa) (the default), the first RSA key
 is read (i.e., non-RSA keys are skipped).

 The following kinds of @rhombus(key) values are supported:

@itemlist(

 @item{A @rhombus(PathString, ~annot) @rhombus(key) is the same as
  @rhombus(ssl.Context.PrivateKey.pem(key)).}

 @item{A @rhombus(ssl.Context.PrivateKey.pem) key loads a key from the
  given path in PEM format.}

 @item{A @rhombus(ssl.Context.PrivateKey.pem_bytes) parses a key from
  the given byte string in PEM format.}

 @item{A @rhombus(ssl.Context.PrivateKey.der) key loads a key from the
  given path in DER/ASN1 format.}

)

 You can use the file @filepath{test.pem} as report by
 @rhombus(collect.file_path(~collect: "openssl", ~file: "test.pem")) for
 testing purposes. Since @filepath{test.pem} is public, such a test
 configuration obviously provides no security.

}


@doc(
  method (ctx :: ssl.Context).load_certificate_chain(
    path :: PathString
  )
  method (ctx :: ssl.Context)
    .load_suggested_certificate_authorities(path :: PathString)
){

 The @rhombus(ssl.Context.load_certificate_chain) method loads a
 PEM-format certification chain file for connections. This chain is used
 to identify the client or server when it connects or accepts
 connections. Loading a chain overwrites the old chain. Also use
 @rhombus(ssl.Context.load_private_key) to load the certificate’s corresponding key.

 The @rhombus(ssl.Context.load_suggested_certificate_authorities) method
 loads a PEM-format file containing certificates that are used by a
 server. The certificate list is sent to a client when the server
 requests a certificate as an indication of which certificates the server
 trusts. Loading the suggested certificates does not imply trust,
 however; any certificate presented by the client will be checked using
 the trusted roots loaded via @rhombus(ssl.Context.load_verify_source).

 You can use the file @filepath{test.pem} as report by
 @rhombus(collect.file_path(~collect: "openssl", ~file: "test.pem")) for
 testing purposes with either of these methods. Since @filepath{test.pem}
 is public, such a test configuration obviously provides no security.

}

@doc(
  method (ctx :: ssl.Context).set_verify(
    mode :: ssl.Context.VerifyMode = #'always
  )

  method (ctx :: ssl.Context).set_verify_hostname(
    mode :: ssl.Context.VerifyMode = #'always
  )

  enum ssl.Context.VerifyMode
  | never
  | try
  | always
){

 The @rhombus(ssl.Context.set_verify) method requires certificate
 verification on the peer SSL connection when @rhombus(mode) is
 @rhombus(#'always). When @rhombus(mode) is @rhombus(#'try), then
 verification is attempted, but a connection is made even when peer
 certificate verification fails; use @rhombus(ssl.Port.is_peer_verified)
 to determine whether verification succeeded. Verifying the certificate
 is not sufficient to prevent attacks by active adversaries, such as
 man-in-the-middle attacks; use @rhombus(ssl.Context.set_verify_hostname)
 in addition.

 The @rhombus(ssl.Context.set_verify_hostname) method requires hostname
 verification of SSL peers of connections. When hostname verification is
 enabled, the hostname associated with a connection is checked against
 the hostnames listed in the peer’s certificate. If the peer certificate
 does not contain an entry matching the hostname, the connection fails.
 Currently, @rhombus(mode) as @rhombus(#'try) is treated the same as
 @rhombus(mode) as @rhombus(#'always). Hostname verification does not
 imply certificate verification; to verify the certificate itself, also
 use @rhombus(ssl.Context.set_verify).

 Enabling verification also requires, at a minimum, designating trusted
 certificate authorities with @rhombus(ssl.Context.load_verify_source).

}


@doc(
  method (ctx :: ssl.Context).set_ciphers(cipher_spec :: String)
){

 Specifies the cipher suites that can be used in connections created
 with context. The meaning of @rhombus(cipher_spec) is the same as for
 the
 @hyperlink("https://docs.openssl.org/master/man1/openssl-ciphers/"){@exec{openssl
   ciphers} command}.

}

@doc(
  method (ctx :: ssl.Context.Server).set_server_alpn(
    alpn_protocols :: List.of(Bytes),
    ~allow_no_match: allow_no_match = #true
  ) :: Void
){

 Sets the @ALPN protocols supported by the server context @rhombus(ctx).
 The protocols are listed within @rhombus(alpn_protocols) in order of
 preference, most-preferred first. That is, when a client connects, the
 server selects the first protocol in its @rhombus(alpn_protocols) that
 is supported by the client.

 If the client does not use ALPN, then the connection is accepted and no
 protocol is selected. If the client uses ALPN but has no protocols in
 common with the server, then if @rhombus(allow_no_match) is true, the
 connection is accepted and no protocol is selected; if
 @rhombus(allow_no_match) is @rhombus(#false), then the connection is
 refused.

}

@doc(
  method (ctx :: ssl.Context).seal() :: Void
){

 Seals @rhombus(ctx), preventing further modifications. After a context
 is sealed, calling a method such as @rhombus(ssl.Context.set_verify) or
 @rhombus(ssl.Context.load_verify_source) throws an exception.

}
