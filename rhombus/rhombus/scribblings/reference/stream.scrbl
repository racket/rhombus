#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Streams}

A @deftech{stream} is a stateless @tech{sequence} that supports
@rhombus(Stream.first) and @rhombus(Stream.rest) operations. Accessing
@rhombus(Stream.first) multiple times for the same stream will produce
the same result, and calling @rhombus(Stream.rest) multiple times for the
same stream will produce the same new stream that omits the first
element. @tech{Lists}, @tech{pair lists}, and sequenceable @tech{ranges}
are streams, as well as lazy streams constructed using
@rhombus(Stream.cons).

A stream is @tech{indexable} using @brackets to access a stream
element by position via @rhombus(#%index).

@doc(
  annot.macro 'Stream'
  annot.macro 'Stream.assume_of($annot, ...)'
){

 Matches any @tech{stream}.

 A @rhombus(Stream.assume_of(annot, ...), ~annot) annotation is the same as
 @rhombus(Stream, ~annot), but elements drawn from the stream via
 @rhombus(Stream.first) have the static information of  the @rhombus(annot)s
 (where multiple @rhombus(annot)s correspond to multiple values for each
 element, such as the key and value from a @tech{map}). The
 extracted elements are not checked, however; each
 @rhombus(annot) is used only for its static information, and
 each @rhombus(annot) must be a @tech(~doc: model_doc){predicate annotation}.

 Static information associated by @rhombus(Stream, ~annot) or
 @rhombus(Stream.assume_of, ~annot) makes an expression acceptable as a
 sequence to @rhombus(for) in static mode.

}

@doc(
  property (stm :: Stream).first :: Any.like_element(stm)
  property (stm :: Stream).rest
    :: Stream.assume_of(Any.like_element(stm))
  method (stm :: Stream).is_empty() :: Boolean
){

 The @rhombus(Stream.first) and @rhombus(Stream.rest) properties report
 the first element of @rhombus(stm) or a new stream that has the rest of
 the elements of @rhombus(stm), respectively.

 The @rhombus(Stream.first) and @rhombus(Stream.rest) properties require
 non-empty streams. The @rhombus(Stream.is_empty) method reports whether
 @rhombus(stm) is empty.

@examples(
  Stream.first([1, 2, 3])
  Stream.rest([1, 2, 3])
  Stream.is_empty([1, 2, 3])
  ~error:
    Stream.first([])
  ~error:
    Stream.rest([])
)

}

@doc(
  method (stm :: Stream).get(n :: Nat) :: Any.like_element(stm)
){

 Equivalent to @rhombus(stm[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th element of
 @rhombus(stm) (starting from @rhombus(0)).

}


@doc(
  def Stream.empty :: Stream
  bind.macro 'Stream.empty'
){

 Returns or matches an empty stream. The empty stream is not unique, so
 check for an empty stream by matching to @rhombus(Stream.empty, ~bind)
 or calling @rhombus(Stream.is_empty).

}


@doc(
  ~nonterminal:
    first_expr: block expr
    rest_expr: block expr
    first_bind: def bind ~defn
    rest_bind: def bind ~defn
  expr.macro 'Stream.cons($maybe_eager $first_expr, $maybe_eager $rest_expr)'
  bind.macro 'Stream.cons($first_bind, $rest_bind)'
  grammar maybe_eager
  | ~eager
  | #,(epsilon)
){

 The @rhombus(Stream.cons) expression form creates a stream whose first
 element is determined by evaluating @rhombus(first_expr) on demand, and
 whose remainder is determined by evaluating @rhombus(rest_expr) on
 demand. After @rhombus(first_expr) or @rhombus(rest_expr) is evaluated,
 its value is retained (and the corresponding expression is forgotten)
 for use by future demands. The result of @rhombus(rest_expr) must
 satisfy @rhombus(Stream, ~annot), but it is checked only at the point
 where the stream's remainder is demanded. The result of @rhombus(first_expr)
 can be multiple values, in which case the corresponding element of the
 stream consists of multiple values.

 If @rhombus(first_expr) or @rhombus(rest_expr) is prefixed with
 @rhombus(~eager), then the expression is evaluated at the time the
 stream is constructed, instead of delaying until the result is demanded.

 The @rhombus(Stream.cons, ~bind) binding form matches a non-empty
 stream, and it binds @rhombus(first_bind) and @rhombus(rest_bind) to the
 first and rest of the matched stream---but not demanding the first or
 rest if @rhombus(first_bind) or @rhombus(rest_bind) is literally
 @rhombus(_, ~bind), respectively.

@examples(
  ~defn:
    fun stream_skip(s :: Stream) :: Stream:
      match s
      | Stream.cons(fst, Stream.cons(_, rst)): Stream.cons(fst, stream_skip(rst))
      | Stream.cons(fst, _): Stream.cons(fst, Stream.empty)
      | Stream.empty: Stream.empty
    def evens = stream_skip(0 ..)
  ~repl:
    evens.first
    evens.rest.first
    evens[100]
)

}
