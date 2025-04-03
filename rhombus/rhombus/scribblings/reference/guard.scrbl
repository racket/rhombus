#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Guards}

@doc(
  ~nonterminal:
    test_expr: block expr
    failure_body: block body
    body: block body
  defn.sequence_macro 'guard $test_expr
                       | $failure_body
                         ...
                       $body
                       ...'
){

 Checks that @rhombus(test_expr) produces a true value, evaluating the
@rhombus(body) sequence if so. If @rhombus(test_expr) produces
@rhombus(#false), then @rhombus(body) is skipped and @rhombus(failure_body)
is evaluated instead. This is equivalent to
@rhombus(if $test_expr | $body ... | $failure_body ...), and is primarily
useful when @rhombus(body) is much more complex than @rhombus(failure_body)
or contains a mixture of definitions and additional @rhombus(guard) forms
interleaved with each other.

Static information works the same way as it would in an equivalent
@rhombus(if) expression.

@examples(
  block:
    guard #true | println("KABOOM!!!")
    println("everything working normally")

  block:
    guard #false | println("KABOOM!!!")
    println("everything working normally")
)

}


@doc(
  ~nonterminal:
    test_bind: block expr
    target_expr: block expr
    target_body: block body
    failure_body: block body
    body: block body
  defn.sequence_macro 'guard.let $test_bind = $target_expr
                       | $failure_body
                         ...
                       $body
                       ...'
  defn.sequence_macro 'guard.let $test_bind:
                         $target_body
                         ...
                       | $failure_body
                         ...
                       $body
                       ...'
){

 Checks that @rhombus(target_expr) produces a value that matches
@rhombus(test_bind) and makes the bindings of @rhombus(test_bind)
available in the subsequent @rhombus(body) sequence. If
@rhombus(target_expr) does not match @rhombus(test_bind), then the
@rhombus(body) sequence is skipped and @rhombus(failure_body) is
evaluated instead. This is the pattern matching variant of
@rhombus(guard), see its documentation for general advice on using
guards.

@examples(
  ~defn:
    fun print_third(xs):
      guard.let [_, _, third, & _] = xs
      | println("list doesn't have three or more elements")
      println(third)
  ~repl:
    print_third(["hi", "hello", "goodbye", "farewell"])
    print_third(["hi", "hello"])
)

The block form with @rhombus(target_body) is
equivalent to using @rhombus(block: target_body ...) as the
@rhombus(target_expr).

@examples(
  ~defn:
    fun print_third(xs):
      guard.let [_, _, third, & _]:
        xs
      | println("list doesn't have three or more elements")
      println(third)
  ~repl:
    print_third(["hi", "hello", "goodbye", "farewell"])
    print_third(["hi", "hello"])
)

}
