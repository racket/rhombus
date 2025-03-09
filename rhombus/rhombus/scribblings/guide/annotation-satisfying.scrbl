#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def ann_eval = macro.make_macro_eval())

@title(~tag: "annotation-satisfying"){Annotations from Predicates}

@Secref("bind-macro") shows @rhombus(matching, ~annot) for turning a
binding pattern into an annotation. The @rhombus(satisfying, ~annot)
annotation constructor wraps an arbitrary predicate function, and it
converts it to an annotation that is satisfied when the predicate
returns a true value.

@examples(
  ~eval: ann_eval
  ~defn:
    fun is_even(x):
      x mod 2 == 0
    fun f(x :: Int && satisfying(is_even)):
      x / 2
  ~repl:
    f(10)
    ~error:
      f(9)
)

The use of @rhombus(&&, ~annot) in this example means that @rhombus(x)
has static information from @rhombus(Int, ~annot). The combination of
@rhombus(&&, ~annot) and @rhombus(satisfying, ~annot) can be especially
useful in defining a new annotation with @rhombus(annot.macro) (see
@secref("bind-macro")).

To recognize specific values via an implicit @rhombus(==) predicate, use
@rhombus(Any.of, ~annot):

@examples(
  ~eval: ann_eval
  ~defn:
    fun add(~from: from :: Any.of(#'left, #'right),
            n, ...):
      if from == #'left
      | math.sum(n, ...)
      | math.sum(& [n, ...].reverse())
  ~repl:
    add(~from: #'left, 0.1, 0.9, 9.007199254740998e15)
    add(~from: #'right, 0.1, 0.9, 9.007199254740998e15)
)

The argument to @rhombus(satisfying, ~annot) or @rhombus(Any.of, ~annot)
is an arbitrary expression, which raises the question of when the
expression is evaluated. The expression is evaluated when the annotation
is encountered, which is not necessarily the same as each time it is
applied. If a binding with the annotation is repeated via
@rhombus(..., ~bind), for example, the annotation may be checked
multiple times against different values, but the expression to construct
the annotation will be evaluated just once. Similarly, an annotation
check that is repeated via @rhombus(List.of, ~annot) or delayed by
@rhombus(Array.later_of, ~annot) can involve a definition evaluated once
when the enclosing annotation is reached, not each time the repeated or
delayed annotation is checked. Still, expressions in an annotation
associated with a function argument must be evaluated each time the
function is called, because because bindings for earlier arguments are
available to the expression.

@examples(
  ~eval: ann_eval
  ~defn:
    fun add_more(a,
                 b :: satisfying(block:
                                   println("getting predicate")
                                   fun (b): b < a),
                 ...):
      math.sum(a, b, ...)
  ~repl:
    add_more(10, 2, 3)
    ~error:
      add_more(1, 2, 3)
)
