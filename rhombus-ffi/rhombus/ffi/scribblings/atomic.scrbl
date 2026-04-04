#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      ffi/atomic
      rhombus/thread open)

@title(~tag: "atomic"){Atomic Mode and Uninterruptible Mode}

@docmodule(ffi/atomic)

@deftech{Atomic mode} evaluates a Rhombus expression without switching among
Rhombus @tech(~doc: ref_doc){coroutine threads} and with limited support
for @tech(~doc: ref_doc){synchronizable events}. An atomic computation
in this sense is not atomic with respect to parallel threads, but only
to coroutine threads. Asynchronous break exceptions (in the sense of
@rhombus(Thread.break)) are also disabled in atomic mode.

Atomic mode is unsafe, because the Rhombus scheduler is not able to
operate while execution is in atomic mode: (1) the scheduler cannot
switch coroutine threads or poll certain kinds of events, which can lead
to deadlock or starvation of other threads; and (2) calling a
scheduler-related function in atomic mode has unspecified behavior,
where misuse is not necessarily caught with a check. Functions that are
directly scheduler-related include @rhombus(Thread),
@rhombus(Thread.sleep), and @rhombus(Evt.sync). Beware that other
operations can involve such synchronization, such as writing to an
output port or using a mutable map. Even if an output target is known to
be free of synchronization, beware that values can have arbitrary
printing procedures attached through @rhombus(Printable, ~class).
Successful use of atomic mode requires a detailed knowledge of any
implementation that might be reached during atomic mode to ensure that
it terminates and does not involve synchronization.

@deftech{Uninterruptible mode} is related to @tech{atomic mode}. It is
also unsafe and practically the same as atomic mode in a coroutine
thread, but uninterruptible mode does not force a parallel thread to
synchronize with all coroutine threads. Uninterruptible mode also allows
the use of uncontested semaphores and mutable maps.

@doc(
  fun atomic.start_atomic() :: Void
  fun atomic.end_atomic() :: Void
  expr.macro 'atomic.atomically:
                $body
                ...'
){

 The @rhombus(atomic.start_atomic) function increments an internal
 atomic-mode counter, starting @tech{atomic mode} if the counter was
 zero, and @rhombus(atomic.end_atomic) decrements the atomic-mode
 counter, ending atomic mode if the counter becomes zero.

 The @rhombus(atomic.atomically) form uses @rhombus(try) with
 @rhombus(atomic.start_atomic) as an @rhombus(~initially) clause and
 @rhombus(atomic.end_atomic) as a @rhombus(~finally) clause.

}

@doc(
  fun atomic.start_uninterruptible() :: Void
  fun atomic.end_uninterruptible() :: Void
  expr.macro 'atomic.uninterruptibly:
                $body
                ...'
){

 Like @rhombus(atomic.start_atomic), @rhombus(atomic.end_atomic), and
 @rhombus(atomic.atomically), but for @tech{uninterruptible mode}. Since
 uninterruptible mode is subsumed by by @tech{atomic mode}, incrementing
 the uninterruptible-mode counter has no effect in atomic mode. In
 contrast, atomic mode must not be started during uninterruptible mode.

}
