#lang rhombus/scribble/manual
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


 Returns the value(s) of the @rhombus(body) sequence, but runs the body or expression of
 an @rhombus(~initially) clause when entering the @rhombus(try) body
 (whether normally or by a @tech{continuation} jump) and the body or expression of a
 @rhombus(~finally) clause when leaving the @rhombus(try) body (whether
 normally or by a @tech{continuation} jump, including exception throws).

 If an exception is thrown during the the @rhombus(body) sequence, the
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
    k(block:
        println("pre")
        "again")
    Continuation.in k:
      println("body")
      "again"
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


@doc(
  fun Exn.Fail.Annot(message :: ReadableString,
                     marks :: Continuation.Marks)
    :: Exn.Fail.Contract
){

 A constructor alias for @rhombus(Exn.Fail.Contract, ~annot), since
 annotation-satisfaction failures exceptions do not have additional
 information beyond @rhombus(Exn.Fail.Contract, ~annot).

}
