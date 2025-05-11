#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/thread:
        only Channel
        open)

@title(~tag: "channel"){Channels}

A @deftech{channel} both synchronizes a pair of threads and passes a
value from one to the other. Channels are synchronous; both the sender
and the receiver block until the (atomic) transaction is
complete. Multiple senders and receivers can access a channel at once,
but a single sender and receiver is selected for each transaction.

In addition to its use with channel-specific procedures, a channel can
be used as a @tech{synchronizable event}.  A channel is
ready for synchronization when @rhombus(Channel.get) would not
block; the channel's synchronization result is the same as the
@rhombus(Channel.get) result.

@doc(
  class Channel()
){

Constructs a channel.  The channel can be used with @rhombus(Channel.get) or as
a @tech{synchronizable event} to receive a value through the channel.  The
channel can be used with @rhombus(Channel.put) or through the result of
@rhombus(Channel.put_evt) to send a value through the channel.

}

@doc(
  method (ch :: Channel).get() :: Any
){

Blocks until a sender is ready to provide a value through @rhombus(ch).  The
result is the sent value.

}

@doc(
  method (ch :: Channel).put(val :: Any) :: Void
){

Blocks until a receiver is ready to accept the value @rhombus(val) through
@rhombus(ch).

}

@doc(
  method (ch :: Channel).put_evt(val :: Any) :: Channel.PutEvt
){

Constructs a @tech{synchronizable event} that is ready for synchronization when
@rhombus(Channel.put(ch, val)) would not block, and the event's
synchronization result is the event itself.

}

@doc(
  annot.macro 'Channel.PutEvt'
){

An annotation that recognizes a @tech{synchronizable event} created by
@rhombus(Channel.put_evt).

}