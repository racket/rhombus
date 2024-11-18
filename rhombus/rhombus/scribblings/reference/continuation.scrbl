#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Continuations}

A @deftech{continuation} represents a captured evaluation context.
Control can ``jump'' into a continuation by delivering values to it,
or by using it with @rhombus(Continuation.in).

@doc(
  annot.macro 'Continuation'
){

 Recognizes continuations as captured by
 @rhombus(Continuation.capture). Continuations are callable as
 functions and satisfy @rhombus(Function, ~annot).

}

@doc(
  ~nonterminal:
    tag_expr: block expr
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
    $bind
    ($bind, ...)
){

 Returns the value(s) of the @rhombus(body) sequence, but also establishes
 a delimiting continuation prompt around the sequence. If
 @rhombus(tag_expr) is present, it determines the tag used for the
 prompt, otherwise @rhombus(Continuation.PromptTag.default) is used.

 The @rhombus(~catch) clauses is superficially similar to
 @rhombus(~catch) in @rhombus(try), but @rhombus(~catch) in
 @rhombus(Continuation.prompt) does not catch exceptions. Instead, it
 determines a handler that is used to receive any values delivered to the
 prompt via @rhombus(Continuation.escape). The handler is call with the
 continuation of the @rhombus(Continuation.prompt) form. Since multiple
 values can be delivered by an escape, the @rhombus(~catch) construction
 can accept multiple values or dispatch on the number of values received.
 The default prompt handler expects a single thunk, and calls the thunk
 under a prompt with the same tag as the handler's prompt.

}

@doc(
  ~nonterminal:
    tag_expr: block expr
  expr.macro 'Continuation.capture $maybe_tag_expr $id:
                $body
                ....'
  grammar maybe_tag_expr:
    $tag_expr
    #,(epsilon)
){

 Captures the continuation of the @rhombus(Continuation.capture)
 expression, binds it to @rhombus(id), and then evaluates the
 @rhombus(body) sequence in tail position. The continuation is
 callable as a function that accepts values to deliver to the
 continuation, but it can also be used with @rhombus(Continuation.in).

 The captured continuation is delimited by a prompt with the tag
 specified by @rhombus(tag_expr), where
 @rhombus(Continuation.PromptTag.default) is used if
 @rhombus(tag_expr) is not present. A prompt with the designated tag
 must be present in the current continuation at the time of capture.

 The captured continuation is composable, which means that the capture
 continuation extends the current one when it is called, and no prompt
 is required in the continuation of the call to the capture
 continuation.

}


@doc(
  ~nonterminal:
    cont_expr: block expr
  expr.macro 'Continuation.in $cont_expr:
                $body
                ...'
){

 Evaluates the @rhombus(body) sequence ``in'' the captured
 continuation produced by @rhombus(cont_expr), as opposed to
 delivering values to it. More precisely, the current continuation is
 extended with the captured continuation, and the @rhombus(body)
 sequence is evaluated with that as the continuation.

}


@doc(
  fun Continuation.escape(
    ~tag: tag :: Continuation.PromptTag:
            Continuation.PromptTag.default,
    val :: Any, ...
  ) :: None
){

 Escapes to the nearest prompt in the current continuation that has the
 prompt tag @rhombus(tag), delivering the @rhombus(val)s to the prompt's
 handler.

}

@doc(
  annot.macro 'Continuation.PromptTag'
){

 Recognizes prompt tags as produced by
 @rhombus(Continuation.PromptTag.make).

}

@doc(
  fun Continuation.PromptTag.make(
    name :: maybe(ReadableString || Symbol) = #false
  ) :: Continuation.PromptTag
  def Continuation.PromptTag.default
    :: Continuation.PromptTag
){

 Creates a fresh prompt tag or accesses a default prompt tag.

 If @rhombus(name) is provided to
 @rhombus(Continuation.PromptTag.make), it is used only for printing and
 other debugging purposes.

}

@doc(
  expr.macro 'Continuation.barrier:
                $body
                ...'
){

 Like @rhombus(block), but disallowing a continuation capture during the
 @rhombus(body) that extends to the context of the
 @rhombus(Continuation.barrier) form.

@examples(
  ~error:
    Continuation.barrier:
      Continuation.capture k:
        k
  Continuation.barrier:
    Continuation.prompt:
      Continuation.capture k:
        k
)

}



@doc(
  annot.macro 'Continuation.Marks'
){

 Recognizes continuation marks as returned by
 @rhombus(Continuation.Marks.current).

}


@doc(
  fun Continuation.Marks.current() :: Continuation.Marks
){

 Returns the marks of the current continuation.

}


@doc(
  ~nonterminal:
    key_expr: block expr
    val_expr: block expr
  expr.macro 'Continuation.with_mark $key_expr = $val_expr:
                $body
                ...'
){

 Sets the current frame's continuation mark for the result of
 @rhombus(key_expr) to the result of @rhombus(val_expr) and evaluates the
 @rhombus(body) sequence in tail position.

}


@doc(
  fun Continuation.call_with_immediate_mark(
    key :: Any,
    ~default: default :: Any = #false,
    fn :: Function.of_arity(1)
  )
){

 Calls @rhombus(fn) in tail position, providing as its argument the
 current frame's mark value for @rhombus(key), or @rhombus(default) if the
 current frame has no mark for @rhombus(key).

}
