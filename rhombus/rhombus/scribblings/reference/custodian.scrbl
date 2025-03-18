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

A @deftech{custodian box} created with @rhombus(Custodian.Box)
strongly holds onto a value placed in the box until the box's
custodian is shut down.

@doc(
  class Custodian():
    constructor (~parent: cust :: Custodian = Custodian.current())
){

 Represents a @tech{custodian}.

 A new custodian is always created with some existing custodian as its
 parent. If the parent custodian is shut down, then the child is shut
 down, too.

}


@doc(
  method (cust :: Custodian).shutdown_all() :: Void
  method (cust :: Custodian).is_shutdown() :: Boolean
){

 The @rhombus(Custodian.shutdown_all) method closes all objects managed
 by @rhombus(cust).

 The @rhombus(Custodian.is_shutdown) method reports whether a custodian
 has been shut down already. A custodian that has been shut down
 cannot become the owner of new objects.

}


@doc(
  Parameter.def Custodian.current :: Custodian
){

 A @tech{context parameter} that determines the custodian for newly
 created object such as threads and file-stream ports.

}


@doc(
  class Custodian.Box():
    constructor (v :: Any,
                 ~custodian: cust :: Custodian = Custodian.current())
){

 Returns a @tech{custodian box} that contains @rhombus(v) as long as
 @rhombus(cust) has not been shut down. If @rhombus(cust) is already
 shut down, the custodian box's value is immediately removed.

}


@doc(
  property (bx :: Custodian.Box).value :: Any
){

 Returns the value in the given custodian box @rhombus(bx), or
 @rhombus(#false) if the value has been removed.

}
