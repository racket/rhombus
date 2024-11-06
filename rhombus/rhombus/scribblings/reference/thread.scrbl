#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@title(~tag: "thread"){Threads}

@doc(
  class Thread():
    constructor (thunk :: Function.of_arity(0))
  expr.macro 'thread:
                $body
                ...'
){

 The @rhombus(Thread, ~class) class represents a concurrent thread of
 evaluation. The @rhombus(Thread) constructor accepts a function
 to call in a newly created thread. A @rhombus(thread: body; ...) form is
 equivalent to @rhombus(Thread(fun (): body; ...)).

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
