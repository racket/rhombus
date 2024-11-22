#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Closeables}

The @rhombus(Closeable.let) definition form binds a name to a @rhombus(Closeable, ~class)
object and injects a call to its @rhombus(close, ~datum) method to close the
object after subsequent definitions and expressions. Implementing the
@rhombus(Closeable, ~class) interface allows an object to cooperate with
@rhombus(Closeable.let).

@doc(
  ~nonterminal:
    rhs_expr: block expr
  defn.sequence_macro 'Closeable.let $bind = $rhs_expr
                       $body
                       ...'
){

 Evaluates @rhombus(rhs_expr) to get a @rhombus(Closeable, ~class) object and defines
 @rhombus(bind) as the result for use in the @rhombus(body) sequence
 (which must be non-empty). After the @rhombus(body) sequence completes,
 the object is closed with its @rhombus(close, ~datum) method before returning the
 results of the @rhombus(body) sequence.

 The @rhombus(rhs_expr) is evaluated with breaks disabled like the
 @rhombus(~initially) part of @rhombus(try). If control escapes from the
 @rhombus(body) sequence (e.g., because an exception is raised), then the
 object produced by @rhombus(rhs_expr) is closed before escaping, and breaks
 are disabled during that close as in the @rhombus(~finally) part of
 @rhombus(try). Consequently, in the case of a break exception, the
 object is reliably closed or not yet opened by @rhombus(rhs_expr). A
 continuation jump back into the @rhombus(body) sequence is disallowed
 via @rhombus(Continuation.barrier).

@examples(
  ~fake:
    block:
      Closeable.let i = Port.Input.open_file("data.txt")
      i.read_line()
      // `i` is closed after this point
    "file content"
)

}

@doc(
  interface Closeable
){

 An interface that a class can implement (publicly or privately) to
 customize the way its objects close, especially with @rhombus(Closeable.let).

 The @rhombus(Closeable, ~class) interface has one method:

@itemlist(

 @item{@rhombus(#,(@rhombus(close, ~datum))()) --- closes the object.
  Closing an already-closed object should succeed without any effect. The
  result should normally be @rhombus(#void), but the result is not
  specifically constrained by the @rhombus(Closeable, ~class) interface.}

)

}

@doc(
  method (c :: Closeable).close()
  fun Closeable.close(c :: Closeable) :: Void
){

 Closes @rhombus(c) using its @rhombus(close, ~datum) implementation.
 The function @rhombus(Closeable.close) ignores the method's results
 and returns @rhombus(#void) regardless.

}
