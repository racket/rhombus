#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Exceptions}

@doc(
  decl.macro 'try:
                $maybe_initially
                $body
                ...
                $maybe_catch
                $maybe_finally='

  grammar maybe_initially:
    ~initially: $body; ...
    #,(epsilon)

  grammar maybe_catch:
    ~catch $binding: $body; ...
    ~catch
    | $binding: $body; ...
    | ...
    #,(epsilon)

  grammar maybe_finally:
    ~finally: $body; ...
    #,(epsilon)
){


 Returns the value of the @rhombus(body) sequence, but runs the body of
 an @rhombus(~initially) clause when entry the @rhombus(try) body
 (whether normally or by a continuation jump) and the body of a
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
      ~initially: displayln("in")
      "ok"
      ~finally: displayln("out")
  ~repl:
    ~error:
      try:
        ~initially: displayln("in")
        1/0
        ~finally: displayln("out")
  ~repl:
    try:
      1/0
      ~catch exn :: Exn.Fail.Contract.DivideByZero:
        "handled"
  ~repl:
    try:
      ~initially: displayln("in")
      1/0
      ~catch _:
        displayln("ignoring all exceptions!")
        0
      ~finally: displayln("out")

  ~repl:
    def k:
      Continuation.prompt:
        try:
          ~initially: displayln("in")
          Continuation.capture k: k
          ~finally: displayln("out")
    k("again")
)

}

@doc(
  decl.macro 'throw $expr'
){

 Throws the value of @rhombus(expr) as an exception. Any value can be
 thrown, but typically thrown values are instances of a subclass of
 @rhombus(Exn).

}

@doc(
  fun error(message :: String)
  fun error(who :: String || Symbol || Identifier || Operator || False, message :: String)
){

 Throws the @rhombus(Exn.Fail) exception with @rhombus(message) as the
 message and @rhombus(Continuation.current_marks()) as the continuation
 marks. If @rhombus(who) is not @rhombus(#false), it is added to the
 beginning of the message, and a @litchar{: } separator is added in
 between.

}

@doc(
  class Exn(message :: String, marks :: Continuation.Marks)
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
  class Exn.Fail.Syntax(exprs :: Listof(Syntax)):
    extends Exn.Fail
  class Exn.Fail.Syntax.Unbound():
    extends Exn.Fail.Syntax
  class Exn.Fail.Syntax.MissingModule(path):
    extends Exn.Fail.Syntax
  class Exn.Fail.Read(srclocs :: Listof(Srcloc)):
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
  class Exn.Fail.Filesystem.Errno(errno :: Pair.of(Symbol, Integer)):
    extends Exn.Fail.Filesystem
  class Exn.Fail.Filesystem.MissingModule(path):
    extends Exn.Fail.Filesystem
  class Exn.Fail.Network():
    extends Exn.Fail
  class Exn.Fail.Network.Version():
    extends Exn.Fail.Network
  class Exn.Fail.Network.Errno(errno :: Pair.of(Symbol, Integer)):
    extends Exn.Fail.Network
  class Exn.Fail.OutOfMemory():
    extends Exn.Fail
  class Exn.Fail.Unsupported():
    extends Exn.Fail
  class Exn.Fail.User():
    extends Exn.Fail
  class Exn.Break(continuation :: continuation):
    extends Exn
  class Exn.Break.HangUp():
    extends Exn.Break
  class Exn.Break.Terminate():
    extends Exn.Break

){

 Primitive exceptions.

}
