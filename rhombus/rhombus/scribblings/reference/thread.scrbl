#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@title(~tag: "thread"){Threads}

A @deftech{thread} is a concurrent thread of evaluation. Rhombus
schedules threads preemptively, but conceptually on the same physical
processor; that is, threads provide concurrency, but not parallelism.
Threads can communicate via shared state, and they can synchronize using
@tech{semaphores} or other @tech{synchronizable events}.

A @rhombus(Thread, ~class) object is itself a synchornizable event that
is ready for synchronization when the thread has terminated. The
synchronization result is just the thread object itself.

@doc(
  class Thread():
    constructor (thunk :: Function.of_arity(0))
  expr.macro 'thread:
                $body
                ...'
){

 The @rhombus(Thread, ~class) class represents a @tech{thread}. The
 @rhombus(Thread) constructor accepts a function to call in a newly
 created thread. A @rhombus(thread: body; ...) form is equivalent to
 @rhombus(Thread(fun (): body; ...)).

 A @rhombus(Thread, ~class) object satisfies @rhombus(Evt, ~annot) and
 includes the @rhombus(Evt.sync) method.

}

@doc(
  method (th :: Thread).wait()
){

 Blocks until the thread @rhombus(th) terminates.

}

@doc(
  method (th :: Thread).break(kind :: Thread.Break = #'interrupt)
  enum Thread.Break:
    interrupt
    hang_up
    terminate
){

 Asynchronously raises an @rhombus(Exn.Break) exception in
 @rhombus(th), assuming that it has not yet terminated. If @rhombus(kind)
 is @rhombus(#'hang_up) or @rhombus(#'terminate), then the exception is
 more specifically @rhombus(Exn.Break.HangUp) or
 @rhombus(Exn.Break.Termminate), respectively.

}

@doc(
  property (th :: Thread).handle
  fun Thread.from_handle(handle) :: Thread
){

 The @rhombus(Thread.handle) property accesses a thread object's
 underlying Racket representation. The @rhombus(Thread.from_handle)
 function constructs a Rhombus thread object from a Racket thread object.

}


@doc(
  fun Thread.sleep(secs :: NonnegReal) :: Void
){

 Causes the current thread to pause for at least @rhombus(secs) seconds.

}
