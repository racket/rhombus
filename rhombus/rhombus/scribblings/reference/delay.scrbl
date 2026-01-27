#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/delay open
      rhombus/thread open)

@(def delay_eval = make_rhombus_eval())
@examples(
  ~eval: delay_eval,
  ~hidden:
    import rhombus/delay open
    import rhombus/thread open
)

@title(~tag: "delay"){Delayed Evaluation}

@docmodule(rhombus/delay)

The @rhombusmodname(rhombus/delay) module implements delayed evaluation.
A @deftech{delay} value contains an expression whose evaluation waits
until the delay is forced with the @rhombus(Delay.force) method. After
the delay's value is forced, further calls to @rhombus(Delay.force)
produce the same result.

@doc(
  annot.macro 'Delay'
  annot.macro 'Delay.expect_of($annot, ...)'
  fun Delay(thunk :: () -> ~any) :: Delay
  expr.macro 'delay:
                $maybe_kind
                $body
                ...'
  grammar maybe_kind
  | ~sync
  | #,(epsilon)
){

 The @rhombus(Delay, ~annot) annotation is satisfied by @tech{delay}
 values, which are normally created using the @rhombus(delay) form. The
 @rhombus(Delay.expect_of, ~annot) annotation causes the
 @rhombus(Delay.force) result of an annotated expression to have the
 static information @rhombus(values(annot, ...)) (where multiple
 @rhombus(annot)s correspond to multiple values produced by the delay's
 expression). The forced values are not checked or converted, however,
 and each @rhombus(annot) is used only for its static information.

 The @rhombus(Delay) function creates a delay given a function of zero
 arguments to be called on demand to produce the value.

 The @rhombus(delay) form creates a delay that evaluates the
 @rhombus(body) sequence on demand to obtain the delay's value.

 A delay is @emph{not} thread-safe by default: concurrent attempts to
 force a delay can trigger an exception. Specify @rhombus(~sync) before
 the body of a @rhombus(delay) form to create a delay that is
 thread-safe, in which case concurrent attempts to force the delay's
 value causes all but one thread to wait until the result is ready.

@examples(
  ~eval: delay_eval,
  def d:
    delay:
      println("working")
      1 + 2
  d.force()
  d.force()
)

}

@doc(
  method (dly :: Delay).force()
){

 Return the value(s) of a @tech{delay}, which is the result of the
 @rhombus(body) sequence in a @rhombus(delay) form, for example.

 Calling the @rhombus(Delay.force) method on a delay that is currently
 being forced throws a ``reentrant promise'' exception. Use the
 @rhombus(~sync) kind in @rhombus(delay) to create a delay that can be
 forced concurrently by multiple threads.

@examples(
  ~eval: delay_eval,
  def d:
    delay:
      "ok"
  d.force()
  def d2:
    delay:
      d2.force()
  ~error:
    d2.force()
)

}

@doc(
  method (dly :: Delay).evt(
    ~pool: pool :: maybe(thread.Thread.Pool || Any.of(#'own))
             = #false
  ) :~ Evt
){

 Forces @rhombus(dly) in a fresh thread and returns a
 @tech{synchronizable event} whose value, when the force is complete, is
 the result of @rhombus(dly).

 The @rhombus(pool) argument is passed along to a use of
 @rhombus(thread), so it can be @rhombus(#'own) or a
 @rhombus(ThreadPool, ~annot) to force the delay in parallel, instead of
 merely concurrently.

@examples(
  ~eval: delay_eval,
  def d1:
    delay:
      1
  def d2:
    delay:
      Evt.system_idle.sync()
      2
  Evt.sync(d1.evt(), d2.evt())
)


}

@doc(
  method (dly :: Delay).is_forced() :: Boolean
  method (dly :: Delay).is_running() :: Boolean
){

 The @rhombus(Delay.is_forced) method reports whether @rhombus(dly) has
 been forced already, in which case @rhombus(dly.force()) will
 immediately return the delay's value(s).

 The @rhombus(Delay.is_running) method reports whether @rhombus(dly) is
 currently being forced, in which case @rhombus(dly.force()) will throw
 an exception.

}


@doc(
  property (dly :: Delay).handle :: Any
  fun Delay.from_handle(handle) :: Delay
){

 The @rhombus(Delay.handle) property returns a Racket promise that
 corresponds to the @tech{delay}. The @rhombus(Delay.from_handle)
 function creates a delay from a Racket promise.

}

@(close_eval(delay_eval))
