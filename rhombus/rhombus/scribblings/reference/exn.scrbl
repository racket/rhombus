#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Exceptions}

@doc(
  ~nonterminal:
    escape_id: block id

  expr.macro 'try $maybe_escape:
                $maybe_initially
                $body
                ...
                $maybe_result
                $body
                ...
                $maybe_catch
                $maybe_finally'

  grammar maybe_escape
  | ~escape_as $escape_id
  | #,(epsilon)

  grammar maybe_initially
  | ~initially: $body; ...
  | ~initially $expr
  | #,(epsilon)

  grammar maybe_result
  | ~result: $body; ...
  | ~result $expr
  | #,(epsilon)

  grammar maybe_catch
  | ~catch $bind: $body; ...
  | ~catch
    | $bind: $body; ...
    | ...
  | #,(epsilon)

  grammar maybe_finally
  | ~finally: $body; ...
  | ~finally $expr
  | #,(epsilon)
){


 Returns the value(s) of the @rhombus(body) sequence or the @rhombus(~result) body,
 but runs the body or expression of
 an @rhombus(~initially) clause when entering the @rhombus(try) body
 (whether normally or by a @tech{continuation} jump) and the body or expression of a
 @rhombus(~finally) clause when leaving the @rhombus(try) body (whether
 normally or by a @tech{continuation} jump, including exception throws).

 If a @rhombus(~result) clause is present, it provides the result of the
 @rhombus(try) form. The @rhombus(body) sequence before or after
 @rhombus(~result) can be empty. Bindings after @rhombus(~result) are not
 visible to the @rhombus(~result) body or to @rhombus(body) forms before
 @rhombus(~result).

@examples(
  ~repl:
    try:
      println("in")
      ~result: values("ok", "done")
      println("out")
)

 If an exception is thrown during the the @rhombus(body) sequence, the
 control escapes to the context of the @rhombus(try) @rhombus(body)
 sequence (i.e., ``inside'' the @rhombus(~initially) and
 @rhombus(~finally) guards) and the @rhombus(~catch) cases are tried in
 order. When a @rhombus(~catch) binding matches, then the result of the
 @rhombus(try) form is the body of the @rhombus(~catch) clause. If no
 @rhombus(~catch) clause matches, the exception is re-thrown. Breaks are
 disabled while attempting to match a @rhombus(~catch) clause or
 evaluating its body.

@examples(
  ~repl:
    try:
      ~initially: println("in")
      "ok"
      ~finally: println("out")
  ~repl:
    ~error:
      try:
        ~initially: println("in")
        1/0
        ~finally: println("out")
  ~repl:
    try:
      1/0
      ~catch exn :: Exn.Fail.Contract.DivideByZero:
        "handled"
  ~repl:
    try:
      ~initially: println("in")
      1/0
      ~catch _:
        println("ignoring all exceptions!")
        0
      ~finally: println("out")
  ~repl:
    def k:
      Continuation.prompt:
        try:
          ~initially: println("in")
          Continuation.capture k: k
          ~finally: println("out")
    k(block:
        println("pre")
        "again")
    Continuation.in k:
      println("body")
      "again"
)

 If @rhombus(~escape_as escape_id) is before the block after
 @rhombus(try), then @rhombus(escape_id) is bound for use in the body of
 the @rhombus(try) form as an @deftech{escape continuation} function that
 jumps out of the @rhombus(try) form. The arguments provided to the
 function are returned as the results of the @rhombus(try) form. Calling
 @rhombus(escape_id) is an error when outside the dynamic extent of
 evaluating the @rhombus(try) form.

@examples(
  ~repl:
    try ~escape_as escape:
      1 + escape(0) + 2
      println("doesn't get here")
)

 The last @rhombus(body) form of @rhombus(try) is in @tail_position with
 respect to @rhombus(try) only when no
 @rhombus(~escape_as), @rhombus(~initially), @rhombus(~result),
 @rhombus(~catch), or @rhombus(~finally)
 is present. If none are present, the @rhombus(try) form is the same as
 @rhombus(block).

}

@doc(
  expr.macro 'throw $expr'
){

 Throws the value of @rhombus(expr) as an exception. Any value can be
 thrown, but typically thrown values are instances of a subclass of
 @rhombus(Exn, ~class).

 Since it does not return a value, a @rhombus(throw) expression has the
 static information of @rhombus(None, ~annot).

}


@doc(
  class Exn(message :: ReadableString, marks :: Continuation.Marks)
  class Exn.Fail():
    extends Exn
  class Exn.Fail.Contract():
    extends Exn.Fail
  class Exn.Fail.Contract.Arity():
    extends Exn.Fail.Contract
  class Exn.Fail.Contract.DivideByZero():
    extends Exn.Fail.Contract
  class Exn.Fail.Contract.NonFixnumResult():
    extends Exn.Fail.Contract
  class Exn.Fail.Contract.Continuation():
    extends Exn.Fail.Contract
  class Exn.Fail.Contract.Variable(id :: Symbol):
    extends Exn.Fail.Contract
  class Exn.Fail.Annot(srclocs :: PairList.of(Srcloc)):
    extends Exn.Fail.Contract
  class Exn.Fail.Syntax(exprs :: PairList.of(Syntax)):
    extends Exn.Fail
  class Exn.Fail.Syntax.Unbound():
    extends Exn.Fail.Syntax
  class Exn.Fail.Syntax.MissingModule(path):
    extends Exn.Fail.Syntax
  class Exn.Fail.Read(srclocs :: PairList.of(Srcloc)):
    extends Exn.Fail
  class Exn.Fail.Read.EOF():
    extends Exn.Fail.Read
  class Exn.Fail.Read.NonChar():
    extends Exn.Fail.Read
  class Exn.Fail.Filesystem():
    extends Exn.Fail
  class Exn.Fail.Filesystem.Exists():
    extends Exn.Fail.Filesystem
  class Exn.Fail.Filesystem.Version():
    extends Exn.Fail.Filesystem
  class Exn.Fail.Filesystem.Errno(errno :: Pair.of(Symbol, Int)):
    extends Exn.Fail.Filesystem
  class Exn.Fail.Filesystem.MissingModule(path):
    extends Exn.Fail.Filesystem
  class Exn.Fail.Network():
    extends Exn.Fail
  class Exn.Fail.Network.Errno(errno :: Pair.of(Symbol, Int)):
    extends Exn.Fail.Network
  class Exn.Fail.OutOfMemory():
    extends Exn.Fail
  class Exn.Fail.Unsupported():
    extends Exn.Fail
  class Exn.Fail.User():
    extends Exn.Fail
  class Exn.Break(continuation :: Continuation):
    extends Exn
  class Exn.Break.HangUp():
    extends Exn.Break
  class Exn.Break.Terminate():
    extends Exn.Break

){

 Primitive exceptions.

}
