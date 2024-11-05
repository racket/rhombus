#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@title(~tag: "semaphore"){Semaphore}

@doc(
  class Semaphore():
    constructor (init :: NonnegInt = 0)
){

 Constructs a semaphore that initially has @rhombus(init)
 @rhombus(Semaphore.post) actions.

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
