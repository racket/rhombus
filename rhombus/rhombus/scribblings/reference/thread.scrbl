#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@(def thread_eval = make_rhombus_eval())
@examples(
  ~eval: thread_eval,
  ~hidden:
    import rhombus/thread open
)

@title(~tag: "thread"){Threads}

A @deftech{thread} is a concurrent thread of evaluation. Rhombus
schedules threads preemptively, but conceptually on the same physical
processor as a @deftech{coroutine thread} by default; coroutine threads
provide concurrency, but not parallelism. Create a @deftech{parallel
 thread} by specifying a @tech{parallel thread pool} when creating the
thread. Threads can communicate via shared state, and they can synchronize using
@tech{semaphores} or other @tech{synchronizable events}.

A @rhombus(Thread, ~class) object is itself a synchronizable event that
is ready for synchronization when the thread has terminated. The
synchronization result is just the thread object itself.

@doc(
  class Thread():
    constructor (
      thunk :: Function.of_arity(0),
      ~pool: pool :: maybe(Thread.Pool || Any.of(#'own)) = #false,
      ~keep: keep :: maybe(Thread.Keep) = ~false
    )
  expr.macro 'thread:
                $option; ...
                $body
                ...'
  grammar option
  | ~pool: $pool_body; ...
  | ~keep: $keep_body; ...
  enum Thread.Keep
  | results

){

 The @rhombus(Thread, ~class) class represents a @tech{thread}. The
 @rhombus(Thread) constructor accepts a function to call in a newly
 created thread.

 If @rhombus(pool) is @rhombus(#false), then the thread is created as a
 @tech{coroutine thread}. If @rhombus(pool) is a
 @rhombus(Thread.Pool, ~class), then the thread is created as a
 @tech{parallel thread} in the given pool. Supplying @rhombus(#'own) as
 @rhombus(pool) is equivalent to supplying @rhombus(Thread.Pool(1)), but
 also closing the pool with @rhombus(Thread.Pool.close) immediately after
 the new thread is added.

 If @rhombus(keep) is @rhombus(#false), then the result of
 @rhombus(thunk) is ignored. If @rhombus(keep) is @rhombus(#'results),
 then the results from @rhombus(thunk) are saved and returned by
 @rhombus(Thread.wait) on the thread.

 A @rhombus(thread: body; ...) form is equivalent to
 @rhombus(Thread(fun (): body; ...)). Using a @rhombus(~pool) or
 @rhombus(~keep) option in @rhombus(thread) is the same as passing the
 corresponding argument to the @rhombus(Thread, ~class) constructor.

 A @rhombus(Thread, ~class) object satisfies @rhombus(Evt, ~annot) and
 includes the @rhombus(Evt.sync) method.

}

@doc(
  method (th :: Thread).wait(
    fail_k :: Prcedure.of_arity(0) = Function.pass
  ) :: Any
){

 Blocks until the thread @rhombus(th) terminates.

 If the thread completes normally---without throwing an exception of
 otherwise aborting to the thunk's initial
 @tech(~doc: model_doc){prompt}---then the results are any results saved
 by the thread (because it was created with @rhombus(~keep: #'results))
 or @rhombus(#void) if results are not kept.

 If the thread does not complete normally, then @rhombus(fail_k) is
 called in tail position, so its results are the results of
 @rhombus(Thread.wait).

@examples(
  ~eval: thread_eval
  ~version_and_later "8.18.0.11":
    def th:
      thread:
        ~keep: #'results
        1 + 2
    th.wait()
)

}

@doc(
  method (th :: Thread).break(
    kind :: Thread.Break = #'interrupt
  ) :: Void
  enum Thread.Break
  | interrupt
  | hang_up
  | terminate
){

 Asynchronously throws an @rhombus(Exn.Break) exception in
 @rhombus(th), assuming that it has not yet terminated. If @rhombus(kind)
 is @rhombus(#'hang_up) or @rhombus(#'terminate), then the exception is
 more specifically @rhombus(Exn.Break.HangUp) or
 @rhombus(Exn.Break.Terminate), respectively.

}


@doc(
  method (th :: Thread).kill() :: Void
){

 Terminates the thread @rhombus(th). If the thread has already
 terminated, then @rhombus(Thread.kill) has no effect.

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

@doc(
  class Thread.Pool():
    constructor(n :: PosInt = Thread.Pool.processor_count())
  method Thread.Pool.close() :: Void
  def Thread.Pool.own = #'own
){

 The @rhombus(Thread.Pool, ~class) class represents a @deftech{parallel thread
  pool}. Threads in the same pool share processor resources so that up to
 @rhombus(n) threads run in parallel to each other and other threads. All
 threads (in the pool and otherwise) run concurrently to each other.

 A thread can be added to a pool as long as the pool is still open.
 After @rhombus(Thread.Pool.close) is called on a pool, no new threads
 can be added to the pool, and the pool's resources are released back to
 the system a threads already within the pool terminate.

 The @rhombus(Thread.Pool.own) value @rhombus(#'own) is useful with
 @rhombus(~pool) in @rhombus(thread).

}

@doc(
  fun Thread.Pool.processor_count() :: PosInt
){

 Returns the total number of processors available on the current
 machine. A thread pool larger than this result is unlikely to offer any
 benefit over a pool whose size matches the result.

}

@doc(
  property (th :: Thread.Pool).handle
  fun Thread.Pool.from_handle(handle) :: Thread.Pool
){

 The @rhombus(Thread.Pool.handle) property accesses a thread pool
 object's underlying Racket representation. The
 @rhombus(Thread.from_handle) function constructs a Rhombus thread pool
 object from a Racket thread pool object.

}

@close_eval(thread_eval)
