#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Continuations}

@doc(
  annot.macro 'Continuation'
){

 Recognizes continuations as captured by @rhombus(Continuation.capture).

}

@doc(
  expr.macro 'Continuation.prompt $maybe_tag_expr:
                $body
                ...
                $maybe_catch'

  grammar maybe_tag_expr:
    $tag_expr
    #,(epsilon)

  grammar maybe_catch:
    ~catch $arg_bindings: $body; ...
    ~catch
    | $arg_bindings: $body; ...
    | ...
    ~catch: $entry_point
    #,(epsilon)

  grammar arg_bindings:
    $binding
    ($binding, ...)
){

 Returns the value of the @rhombus(body) sequence, but also establishes
 a delimiting continuation prompt around the sequence. If
 @rhombus(tag_expr) is present, is determines the tag used for the
 prompt, otherwise @rhombus(Continuation.default_prompt_tag) is used.

 The @rhombus(~catch) clauses is superficially similar to
 @rhombus(~catch) in @rhombus(try), but @rhombus(~catch) in
 @rhombus(Continuation.prompt) does not cath exceptions. Instead, it
 determines a handler that is used to receive any values delivered to the
 prompt via @rhombus(Continuation.escape). The handler is call with the
 continuation of the @rhombus(Continuation.prompt) form. Since mutiple
 values can be delivered by an escape, the @rhombus(~catch) construction
 can accept mutiple values or dispatch on the number of values received.
 The default prompt handler expects a single thunk, and calls the thunk
 under a prompt with the same tag as the handler's prompt.

}

@doc(
  expr.macro 'Continuation.capture $maybe_tag_expr $identifier:
                $body
                ....'
  grammar maybe_tag_expr:
    $tag_expr
    #,(epsilon)
){

 Captures the continuation of the @rhombus(Continuation.capture)
 expression, binds it to @rhombus(identifier), and then evaluates the
 @rhombus(body) sequence in tail position. The captured continuation is
 composable and delimited, and it is represented as a procedure that
 accepts values to deliver to the continuation.

 If @rhombus(tag_expr) is present, is determines the tag used to
 delimite the continuation, otherwise
 @rhombus(Continuation.default_prompt_tag) is used. A prompt with the
 designated tag must be present in the current continuation at te time of
 capture. Since the captured continuation is composable, no prompt is
 required for invocation.

}


@doc(
  fun Continuation.escape(val, ...,
                          ~tag: tag = Continuation.default_prompt_tag)
){

 Escapes to the nearest prompt in the current continuation that has the
 prompt tag @rhombus(tag), delivering the @rhombus(val)s to the prompt's
 handler.

}

@doc(
  fun Continuation.make_prompt_tag(name :: String || Symbol || False)
  def Continuation.default_prompt_tag
){

 Creates a fresh prompt tag or accesses a default prompt tag.

 If @rhombus(name) is provided to
 @rhombus(Continuation.make_prompt_tag), it is used only for printing and
 other debugging purposes.

}

@doc(
  annot.macro 'Continuation.Marks'
){

 Recognizes continuation marks as returned by
 @rhombus(Continuation.current_marks).

}


@doc(
  fun Continuation.current_marks() :: Continuation.Marks
){

 Returns the marks of the current continuation.

}
