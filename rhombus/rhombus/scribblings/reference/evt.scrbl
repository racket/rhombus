#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread open)

@title(~tag: "evt"){Synchronizable Events}

A @deftech{synchronizable events} is an object that can be used with
@rhombus(Evt.sync) to wait until it is ready for synchronization.
Synchronizing a ready event may have a side effect and an associated
value. For example, synchronizing on a @rhombus(Semaphore) is the same
as using @rhombus(Semaphore.wait), so it decrements the semaphore's
count.

@doc(
  annot.macro 'Evt'
){

 An annotation that recognizes @tech{synchronizable events}, which
 include @tech{threads}, @tech{semaphores}, an objects returned by
 methods like @rhombus(TCPListener.accept_evt).

 The @rhombus(Evt, ~annot) annotation interface-like in the sense that
 every @rhombus(Evt, ~annot) supports the @rhombus(Evt.sync) method.

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
  annot.macro 'ProgressEvt'
  annot.macro 'CommitEvt'
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
