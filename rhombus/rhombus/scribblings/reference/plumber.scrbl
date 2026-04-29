#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/plumber open)

@title(~tag: "plumber"){Plumbers}

@docmodule(~open, rhombus/plumber)

A @deftech{plumber} supports @deftech{flush callbacks}, which are
normally triggered just before a Rhombus process exits.
For example, a @tech{flush callback} might flush an output port's
buffer.@margin_note{@tech{Flush callbacks} are roughly analogous to the
 standard C library's @as_indexed{@tt{atexit}}, but flush callback can also
 be used in other, similar scenarios.}

There is no guarantee that a flush callback will be called before a
process terminates---either because the plumber is not the original
plumber that is flushed on exit, or because the process is terminated
forcibly (e.g., through a custodian shutdown).

@doc(
  class Plumber()
  class Plumber.FlushHandle():
    constructor ~error
){

 The @rhombus(Plumber, ~class) class represents a @tech{plumber}.

 The @rhombus(Plumber.FlushHandle, ~class) class represents a
 @deftech{flush handle}, which records a particular registration of a
 @tech{flush callback}.

}

@doc(
  Parameter.def Plumber.current :: Plumber
){

 A @tech{context parameter} that determines a @deftech{current plumber}
 for @tech{flush callbacks}. For example, creating an output file
 @rhombus{port} registers a @tech{flush callback} with the @tech{current
  plumber} to flush the port as long as the port is opened.

}

@doc(
  method (plumber :: Plumber).flush_all() :: Void
){

 Calls all @tech{flush callbacks} that are registered with
 @rhombus(plumber).

 The @tech{flush callbacks} to call are collected from @rhombus(plumber)
 before the first one is called. If a @tech{flush callback} registers a
 new @tech{flush callback}, the new one is @emph{not} called. If a
 @tech{flush callback} raises an exception or otherwise escapes, then the
 remaining @tech{flush callbacks} are not called.

}

@doc(
  method (plumber :: Plumber).add_flush(
    flush :: (Plumber.FlushHandle) -> ~any,
    ~weak: weak :: Any.to_boolean = #false
  ) :: Plumber.FlushHandle
){

 Registers @rhombus(flush) as a @tech{flush callback} with
 @rhombus(plumber), so that @rhombus(flush) is called for
 @rhombus(plumber.flush_all()).

 The result @tech{flush handle} represents the registration of the
 callback, and it can be used with @rhombus(Plumber.FlushHandle.remove)
 to unregister the callback.

 The given @rhombus(flush) is reachable from the @tech{flush handle},
 but if @rhombus(weak) is true, then @rhombus(plumber) retains only a
 @tech(~doc: model_doc){weak reference} to the result @tech{flush handle}
 (and thus @rhombus(flush)).

 When @rhombus(flush) is called as a flush callback, it is passed the
 same value that is returned by @rhombus(Plumber.add_flush) so that
 @rhombus(flush) can conveniently unregister itself. The call of
 @rhombus(flush) is within a @tech(~doc: model_doc){continuation
  barrier}.

}

@doc(
  method (fh :: Plumber.FlushHandle).remove() :: Void
){

 Unregisters the @tech{flush callback} that was registered by the
 @rhombus(Plumber.add_flush) call that produced @rhombus(fh).

 If the registration represented by @rhombus(fh) has been removed
 already, then @rhombus(Plumber.FlushHandle.remove) has no effect.

}
