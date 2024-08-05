#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "conditional"){Conditionals and Pattern-Matching Dispatch}

The @rhombus(&&) and @rhombus(||) operators are short-circuiting ``and''
and ''or'' forms. As in Racket, @rhombus(||) returns the first
non-@rhombus(#false) value, and @rhombus(&&) returns the last
non-@rhombus(#false) value.

@examples(
  1 < 2 && "ok"
)

Comparison operators and @rhombus(!) (for ``not'') have higher
precedence than @rhombus(&&) and @rhombus(||), while @rhombus(&&) has
higher precedence than @rhombus(||). Arithmetic operators have higher
precedence than comparison operators, @rhombus(||), @rhombus(&&), but
they have no precedence relative to @rhombus(!). The @rhombus(==)
operator reports structural equality independent of mutations, while
the @rhombus(===) operator reports object equality. Comparison
operators are non-associative and have no precedence relationship with
each other.

The @rhombus(if) form expects a test expression followed by an
alts-block with two @litchar{|}s. The first @litchar{|} holds the
``then'' branch, and the second @litchar{|} holds the ``else'' branch:

@examples(
  if 1 == 2
  | "same"
  | "different"
)

Although an @rhombus(if) could be nested further in the ``else'' branch
to implement an ``if'' ... ``else if'' ... ``else if'' ... combination,
the @rhombus(cond) form supports that combination better. It expects an
alts-block where each @litchar{|} has a test expression followed by a
block. Evaluating the @rhombus(cond) form dispatches to the block after
first test that produces a non-@rhombus(#false) value. The
@rhombus(~else) keyword can be used in place of a last test.

@examples(
  ~defn:
    fun fib(n):
      cond
      | n == 0: 1
      | n == 1: 1
      | ~else: fib(n-1) + fib(n-2)
  ~repl:
    fib(5)
)

If there's no @rhombus(~else) case and no matching case, then
@rhombus(cond) reports an error at run time (unlike Racket, which
returns void in that case). Note that @rhombus(~else) is a keyword, and
not an identifier. If it were an identifier, then @rhombus(else) might
get bound in some context to @rhombus(#false), which would be confusing.
As another special case, @rhombus(_) is allowed in place of
@rhombus(else); although it is possible to bind @rhombus(_), it takes a
specifical effort because @rhombus(_) is a binding operator.

Although @rhombus(cond) is better than @rhombus(if) for @rhombus(fib),
the @rhombus(match) form is even better. The @rhombus(match) form
expects an expression and then an alts-block where each @litchar{|} has
a binding pattern followed by a block. The @rhombus(match) form
evaluates that first expression, and dispatches to the first block whose
pattern accepts the expression's value. Similar to @rhombus(cond),
@rhombus(match) supports @rhombus(~else) in place of a final binding
pattern, but using the binding operator @rhombus(_) is more common.

@examples(
  ~defn:
    fun fib(n):
      match n
      | 0: 1
      | 1: 1
      | _: fib(n-1) + fib(n-2)
)

This kind of immediate pattern-matching dispatch on a function argument
is common enough that @rhombus(fun) supports it directly, fusing the
function declaration and the pattern match, like this:

@examples(
  ~defn:
    fun
    | fib(0): 1
    | fib(1): 1
    | fib(n): fib(n-1) + fib(n-2)
)

There's no @rhombus(~else) for this fused form, but @rhombus(_, ~bind) can be
useful in catch-call clauses where the argument is not used. Also, the
function name and all relevant argument positions have to be repeated in
every case, but that's often a readable trade-off. Match-dispatching
functions cannot have optional arguments, but different cases
can have different numbers of arguments, and a call will find a matching
case with the right number of arguments.

@examples(
  ~defn:
    fun
    | hello(name):
        "Hello, " +& name    // +& coerces to strings and concatenates
    | hello(first, last):
        hello(first +& " " +& last)
  ~repl:
    hello("World")
    hello("Inigo", "Montoya")
)

To write a result annotation just once in a function definition with
multiple cases, use the function name after @rhombus(fun), then
@rhombus(::) or @rhombus(:~), the annotation, and then the cases:

@examples(
  ~defn:
    fun fib :: PosInt:
    | fib(0): 1
    | fib(1): 1
    | fib(n :: NonnegInt): fib(n-1) + fib(n-2)
)
