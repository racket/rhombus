#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@title(~tag: "semaphore"){Semaphore}

A @deftech{semaphore} is a @tech{synchronizable event} that has an
internal counter. Posting to a semaphore increments its counter, and
waiting on a semaphore blocks until the counter is positive and can be
decremented. As a synchronizable event, a semaphore is ready for
synchronization when its counter is positive; the synchronization result
is just the semaphore itself.

@doc(
  class Semaphore():
    constructor (init :: Nat = 0)
){

 Constructs a semaphore that initially has @rhombus(init)
 @rhombus(Semaphore.post) actions.

 A @rhombus(Semaphore, ~class) object satisfies @rhombus(Evt, ~annot) and
 includes the @rhombus(Evt.sync) method.

}

@doc(
  method (sema :: Semaphore).post() :: Void
  method (sema :: Semaphore).wait() :: Void
  method (sema :: Semaphore).poll() :: Boolean
){

 Posts, waits, or polls a semaphore. When @rhombus(Semaphore.poll)
 successfully waits on a semaphore, it returns @rhombus(#true), otherwise
 it returns @rhombus(#false).

}

@doc(
  property (sema :: Semaphore).handle
  fun Semaphore.from_handle(handle) :: Semaphore
){

 The @rhombus(Semaphore.handle) property accesses a semaphore object's
 underlying Racket representation. The @rhombus(Semaphore.from_handle)
 function constructs a Rhombus semaphore object from a Racket semaphore
 object.

}
