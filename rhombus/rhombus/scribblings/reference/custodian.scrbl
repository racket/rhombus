#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/custodian open)

@title(~tag: "custodian"){Custodians}

@docmodule(~open, rhombus/custodian)

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
  method (cust :: Custodian).current_memory_use() :: Nat
  method (cust :: Custodian).limit_memory(
    limit_amt :: Nat,
    ~shutdown: shutdown_cust :: Custodian = cust
  ) :: Void
){

 The @rhombus(Custodian.current_memory_use) method reports the amount of
 memory in bytes that is currently allocated an attributed to threads
 managed by @rhombus(cust). See also
 @secref(~doc: model_doc, "custodian-model").

 @margin_note{A custodian's limit is checked only after a garbage
  collection, except that it may also be checked during certain large
  allocations that are individually larger than the custodian's limit. A
  single garbage collection may shut down multiple custodians, even if
  shutting down only one of the custodians would have reduced memory use
  for other custodians.}

 The @rhombus(Custodian.limit_memory) method registers a limited-memory
 check. If Rhombus later reaches a state after garbage collection (see
 @secref(~doc: model_doc, "gc-model")) where @rhombus(cust) owns more
 than @rhombus(limit_amt) bytes, then @rhombus(shutdown_cust) is shut
 down.

 @margin_note{New memory allocation will be accounted to the running
  @tech{thread}'s managing custodian. In other words, a
  custodian's limit applies only to the allocation made by the threads
  that it manages.}

 For reliable shutdown, @rhombus(limit_amt) must be much lower than the
 total amount of memory available (minus the size of memory that is
 potentially used and not charged to @rhombus(limit_cust)). Moreover, if
 individual allocations that are initially charged to
 @rhombus(limit_cust) can be arbitrarily large, then
 @rhombus(shutdown_cust) must be the same as @rhombus(cust), so that
 excessively large immediate allocations can be rejected with an
 @rhombus(Exn:Fail:OutOfMemory) exception.

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
