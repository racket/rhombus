#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open
      rhombus/network.TCPListener
      rhombus/measure)

@(def evt_eval = make_rhombus_eval())
@examples(
  ~eval: evt_eval,
  ~hidden:
    import:
      rhombus/thread open
      rhombus/measure
)

@title(~tag: "evt"){Synchronizable Events}

A @deftech{synchronizable event} is an object that can be used with
@rhombus(Evt.sync) to wait until it is ready for synchronization.
Synchronizing a ready event may have a side effect and an associated
value. For example, synchronizing on a @rhombus(Semaphore) is the same
as using @rhombus(Semaphore.wait), so it decrements the semaphore's
count.

@doc(
  annot.macro 'Evt'
){

 An annotation that recognizes @tech{synchronizable events}, which
 include @tech{threads}, @tech{semaphores}, objects returned by
 methods like @rhombus(TCPListener.accept_evt), and objects that implement
 the @rhombus(Synchronizable, ~class) interface.

 The @rhombus(Evt, ~annot) annotation interface-like in the sense that
 every @rhombus(Evt, ~annot) supports the @rhombus(Evt.sync) method.

}

@doc(
  def Evt.always :: Evt
  def Evt.never :: Evt
){

 The @rhombus(Evt.always) @tech{synchronizable event} is always ready
 for synchronization, and its synchronization result is itself.

 The @rhombus(Evt.never) @tech{synchronizable event} is never ready for
 synchronization.

}


@doc(
  method (evt :: Evt).sync(
    ~timeout: timeout_secs :: maybe(NonnegReal) = #false,
    ~enable_break: enable_break :: Any.to_boolean = #false,
    evt :: Evt,
    ...
  ) :: Any
){

 Blocks until at least one of the @rhombus(evt)s is ready for
 synchronization, and returns the ready result. If multiple
 @rhombus(evt)s become ready before one is selected, one of the ready
 @rhombus(evt)s is selected at random.

@examples(
  ~eval: evt_eval
  Evt.always.sync()
  Evt.never.sync(~timeout: 0)
  Evt.never.sync(Evt.always)
)

}

@doc(
  method (evt :: Evt).wrap(
    ~return: return :: Evt.WrapReturn = #'no_break,
    wrapf :: Function
  ) :: Evt

  enum Evt.WrapReturn
  | no_break
  | tail
){

 Creates an @rhombus(Evt, ~annot) that is ready for synchronization when @rhombus(evt)
 is ready for synchronization, but whose synchronization result is determined
 by applying @rhombus(wrapf) to the synchronization result of @rhombus(evt).
 The number of arguments accepted by @rhombus(wrapf) must match the number of
 values for the synchronization result of @rhombus(evt).

@examples(
  ~eval: evt_eval
  Evt.always.wrap(fun (_): "done").sync()
  Evt.always.wrap(fun (_): values("also", "done")).sync()
  Evt.always.wrap(fun (_): 1).wrap(fun (v): v+1).sync()
  Evt.always.wrap(fun (_): values(1, 2)).wrap(fun (u, v): u+v).sync()
)

 If @rhombus(return) is @rhombus(#'no_break), then @rhombus(wrapf) is
 called with breaks (in the sense of @rhombus(Thread.break)) disabled.

 If @rhombus(return) is @rhombus(#'tail), then @rhombus(wrapf) is called
 in tail position with respect to a synchronization request via
 @rhombus(Evt.sync). When the @rhombus(Evt, ~annot) produced by
 @rhombus(Evt.wrap) is wrapped by another @rhombus(Evt.wrap) with
 @rhombus(#'no_break), however, this tail-call behavior is disabled.

@examples(
  ~eval: evt_eval
  :
    // loop runs in contsant space
    recur loop(i = 0):
      if i == 10_000
      | "done"
      | Evt.always.wrap(fun (_): loop(i+1), ~return: #'tail).sync()
)

}

@doc(
  method (evt :: Evt).replace(
    replacef :: Function
  ) :: Evt
){

 Creates an @rhombus(Evt, ~annot) @rhombus(evt_result) that is calls
 @rhombus(replacef) when @rhombus(evt) is ready for synchronization. If
 @rhombus(replacef) returns an @rhombus(Evt, ~annot)
 @rhombus(evt_new, ~var), then @rhombus(evt_result) is ready for
 synchronization when @rhombus(evt_new, ~var) is ready, and
 @rhombus(evt_new, ~var) provides the synchronization result; if
 @rhombus(replacef) results any other result, then @rhombus(evt_result)
 becomes ready immediately, and @rhombus(evt_result) itself is the
 synchronization result.

 The number of arguments accepted by @rhombus(replacef) must match the
 number of values for the synchronization result of @rhombus(evt).

@examples(
  ~eval: evt_eval
  Evt.always.replace(fun (_): Evt.always.wrap(fun (_): "replaced")).sync()
  Evt.always.replace(fun (_): "done").sync()
)

}

@doc(
  fun Evt.guard(make :: () -> Evt) :: Evt
){

 Creates a value that behaves as an @rhombus(Evt, ~annot), but that is
 more precisely characterized as an event generator.

 The result @rhombus(guard, ~var) of @rhombus(Evt.guard) generates an
 event when @rhombus(guard, ~var) is used with @rhombus(Evt.sync) (or
 whenever it is part of a @rhombus(Evt.choice) event used with
 @rhombus(Evt.sync), etc.), where the generated event is the result of
 calling @rhombus(make). The @rhombus(make) function may be called by
 @rhombus(Evt.sync) at most once for a given call to @rhombus(Evt.sync),
 but @rhombus(make) may not be called if a ready event is chosen before
 @rhombus(guard, ~var) is even considered.

@examples(
  ~eval: evt_eval
  def mutable counter = 0
  def g = Evt.guard(fun ():
                      let c = counter
                      counter := counter + 1
                      Evt.always.wrap(fun (_): c))
  g.sync()
  g.sync()
)

}

@doc(
  fun Evt.nack_guard(make :: (Evt) -> Evt) :: Evt
){

 Like @rhombus(Evt.guard), but @rhombus(make) is call with an event
 @rhombus(nack, ~var) that becomes ready for synchronization that the
 point where the result or @rhombus(Evt.nack_guard) is not chosen in a
 synchronization.

@examples(
  ~eval: evt_eval
  def mutable state = 0
  def g = Evt.nack_guard(fun (nack :: Evt):
                           state := state + 1
                           thread:
                             nack.sync()
                             state := state - 1
                           Evt.never)
  g.sync(~timeout: 1)
  Evt.system_idle.sync()
  state
  :
    // might try `g`, or might immediately succeed with `Evt.always`
    g.sync(Evt.always)
  Evt.system_idle.sync()
  state
)

}

@doc(
  fun Evt.poll_guard(make :: (Boolean) -> Evt) :: Evt
){

 Like @rhombus(Evt.guard), but @rhombus(make) is call with a boolean
 indicating whether the resulting @rhombus(Evt, ~annot) will be used
 in a polling synchronization (i.e., with a zero timeout), as opposed
 to a blocking synchronization.

@examples(
  ~eval: evt_eval
  def g = Evt.poll_guard(fun (is_poll):
                           Evt.always.wrap(fun (_): to_string(is_poll)))
  g.sync(~timeout: 0)
  g.sync()
)

}

@doc(
  fun Evt.choice(evt :: Evt, ...) :: Evt
){

 Creates an @rhombus(Evt, ~annot) that is ready whenever one of the
 given @rhombus(evt)s is ready, and with that @rhombus(evt)'s
 synchronization result.

 In other words, supplying the result of @rhombus(Evt.choice) to
 @rhombus(Evt.sync) is the same as supplying all of the individual
 @rhombus(evt)s to the same @rhombus(Evt.sync).

@examples(
  ~eval: evt_eval
  ~fake:
    :
      // result could be either:
      Evt.choice(Evt.always.wrap(fun (_): "left"),
                 Evt.always.wrap(fun (_): "right"))
        .sync()
    "right"
)

}


@doc(
  fun Evt.alarm(
    msecs :: Real,
    ~monotonic: monotonic :: Any.to_boolean = #false
  ) :: Evt
){

 Creates a @tech{synchronizable event} is ready when
 @rhombus(system.milliseconds()) would return a value of @rhombus(msecs)
 or more if @rhombus(monotonic) is @rhombus(#false), or when
 @rhombus(measure.real_milliseconds()) would return a value of
 @rhombus(msecs) or more if @rhombus(monotonic) is true.
 The returned event's synchronization result is itself.

@examples(
  ~eval: evt_eval
  Evt.alarm(system.milliseconds() + 1).sync()
  Evt.alarm(measure.real_milliseconds() + 1, ~monotonic: #true).sync()
)

}

@doc(
  def Evt.system_idle :: Evt
){

 The @rhombus(Evt.system_idle) @tech{synchronizable event} becomes ready
 when no thread can run otherwise. In other words, all threads must be
 suspended or blocked on events with timeouts that have not yet expired.
 Its synchronization result is @rhombus(#void).

 The @rhombus(Evt.system_idle) event is intended primarily for use in
 tests where all concurrency is known.

}

@doc(
  property (evt :: Evt).handle
  fun Evt.from_handle(handle) :: Evt
){

 The @rhombus(Evt.handle) property accesses a synchronizable event's
 underlying Racket representation. The @rhombus(Evt.from_handle)
 function constructs a Rhombus synchronizable event from a Racket
 synchronizable event.

}

@doc(
  annot.macro 'ProgressEvt':
    ~method_fallback: Evt
  annot.macro 'CommitEvt':
    ~method_fallback: Evt
){

 A @rhombus(ProgressEvt, ~annot) is produced by
 @rhombus(Port.Input.Progress.evt) to detect and synchronize reads from
 @tech{input ports} that support progress events.

 A @rhombus(CommitEvt, ~annot) is used in combination with a
 @rhombus(ProgressEvt, ~annot) for @rhombus(Port.Input.Progress.commit).
 A @rhombus(CommitEvt, ~annot) is either a @rhombus(Semaphore, ~annot),
 channel-put event, channel, semaphore-peek event, @rhombus(Evt.always),
 or @rhombus(Evt.never).

}

@doc(
  interface Synchronizable
){
 An interface that a class can implement to make instances of the class usable
 as an @rhombus(Evt, ~annot). When a class that implements
 @rhombus(Synchronizable, ~class) is used with @rhombus(Evt.sync), the
 @rhombus(Synchronizable.as_evt) method is called, and the result is used in
 the synchronization.

 The interface has a single abstract method:

@itemlist(
  @item{@rhombus(#,(@rhombus(as_evt, ~datum))()) --- produces an
  @tech{synchronizable event} that can be used in @rhombus(Evt.sync).}
)

}

@doc(
  method (obj :: Synchronizable).as_evt() :: Evt
){

 Obtains a @tech{synchronizable event} for a
 @rhombus(Synchronizable, ~class) object.

}

@close_eval(evt_eval)
