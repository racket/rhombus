#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/custodian open)

@title(~tag: "custodian"){Custodians}

@docmodule(rhombus/custodian)

A @deftech{custodian} manages objects such as threads, file-stream
ports, network connections, and other custodians. Whenever a thread,
etc., is created, it is placed under the management of the current
custodian as determined by the @rhombus(Custodian.current) parameter.

@doc(
  class Custodian():
    constructor (~parent: cust :: Custodian = Custodian.current())
){

 Represents a @tech{custodian}.

 A new custodian is always created with some existing custodian as its
 parent. If the parent custodian is shutdown, then the child is shutdown,
 too.

}


@doc(
  method (cust :: Custodian).shutdown_all() :: Void
  method (cust :: Custodian).is_shutdown() :: Boolean
){

 The @rhombus(Custodian.shutdown_all) method closes all objects managed
 by @rhombus(cust).

 The @rhombus(Custodian.is_shutdown) method reports whether a custodian
 has been shutdown already. A custodian that has been shutdown cannot
 become the owner of new objects.

}


@doc(
  Parameter.def Custodian.current :: Custodian
){

 A @tech{context parameter} that determines the custodian for newly
 created object such as threads and file-stream ports.

}
