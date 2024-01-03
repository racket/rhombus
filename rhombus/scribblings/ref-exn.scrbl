#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Exceptions}

@doc(
  expr.macro 'try:
                $maybe_initially
                $body
                ...
                $maybe_catch
                $maybe_finally'

  grammar maybe_initially:
    ~initially: $body; ...
    ~initially $expr
    #,(epsilon)

  grammar maybe_catch:
    ~catch $bind: $body; ...
    ~catch
    | $bind: $body; ...
    | ...
    #,(epsilon)

  grammar maybe_finally:
    ~finally: $body; ...
    ~finally $expr
    #,(epsilon)
){


 Returns the value of the @rhombus(body) sequence, but runs the body or expression of
 an @rhombus(~initially) clause when entry the @rhombus(try) body
 (whether normally or by a continuation jump) and the body or expression of a
 @rhombus(~finally) clause when leaving the @rhombus(try) body (whether
 normally or by a continuation jump, including exception throws).

 If an excepotion is thrown during the the @rhombus(body) sequence, the
 control escapes to the context of the @rhombus(try) @rhombus(body)
 sequence (i.e., ``inside'' the @rhombus(~initially) and
 @rhombus(~finally) guards) and the @rhombus(~catch) cases are tried in
 order. When a @rhombus(~catch) binding matches, then the result of the
 @rhombus(try) form is the body of the @rhombus(~catch) clause. If no
 @rhombus(~catch) clause matches, the exception is re-thrown. Breaks are
 disabled while attempting to match a @rhombus(~catch) clause or
 evaluating its body.

 The last @rhombus(body) form of @rhombus(try) are not in tail position
 is any of @rhombus(~initially), @rhombus(~catch), or @rhombus(~finally)
 is present. If none are present, the @rhombus(try) form is the same as
 @rhombus(begin).

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
    k("again")
)

}

@doc(
  expr.macro 'throw $expr'
){

 Throws the value of @rhombus(expr) as an exception. Any value can be
 thrown, but typically thrown values are instances of a subclass of
 @rhombus(Exn, ~class).

}

@doc(
  fun error(message :: ReadableString)
    :: None
  fun error(
    who :: maybe(ReadableString || Symbol || Identifier || Operator),
    message :: ReadableString
  ) :: None
){

 Throws the @rhombus(Exn.Fail, ~class) exception with @rhombus(message) as the
 message and @rhombus(Continuation.Marks.current()) as the continuation
 marks. If @rhombus(who) is not @rhombus(#false), it is added to the
 beginning of the message, and a @litchar{: } separator is added in
 between.

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
  class Exn.Fail.Syntax(exprs :: List.of(Syntax)):
    extends Exn.Fail
  class Exn.Fail.Syntax.Unbound():
    extends Exn.Fail.Syntax
  class Exn.Fail.Syntax.MissingModule(path):
    extends Exn.Fail.Syntax
  class Exn.Fail.Read(srclocs :: List.of(Srcloc)):
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
  class Exn.Fail.Network.Version():
    extends Exn.Fail.Network
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
