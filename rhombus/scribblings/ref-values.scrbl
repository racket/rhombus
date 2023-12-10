#lang scribble/rhombus/manual
@(import: "common.rhm"open
          "nonterminal.rhm" open)

@title{Multiple Values}

@doc(
  fun values(v, ...)
){

 Returns the @rhombus(v)s as multiple result values.

 If only one @rhombus(v) is provided, the result is the same as just
 @rhombus(v). Any other number of values must be received by a context
 that is expecting multiple values, such as with a
 @rhombus(values, ~bind) binding pattern.

}

@doc(
  ~nonterminal:
    lhs_bind: def ~defn
  bind.macro 'values($bind, ...)'
){

 The @rhombus(values, ~bind) binding operator can only be used in
 places where it's specifically recognized, normally to match multiple
 result values. For example, the @rhombus(lhs_bind) position of
 @rhombus(def) recognizes @rhombus(values, ~bind).

@examples(
  def values(x, y) = values(1, 2)
  x+y
)

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun ~defn
  annot.macro 'values($annot, ...)'
){

 The @rhombus(values, ~annot) annotation can only be used in places
 where it's specifically recognized, normally to annotate multiple
 result values. For example, the @rhombus(maybe_res_annot) position of
 @rhombus(fun) recognizes @rhombus(values, ~annot).

@examples(
  fun two_numbers() :: values(Int, Int):
    values(1, 2)
  def (x, y) = two_numbers()
  x+y
)

}

@doc(
  reducer.macro 'values($id = $expr, ...)'
){

 A @tech{reducer} used with @rhombus(for), expects as many results from a
 @rhombus(for) body as @rhombus(id)s. For the first iteration of
 the @rhombus(for) body, each @rhombus(id)'s value is the result
 of the corresponding @rhombus(expr). The results of a @rhombus(for) body
 for one iteration then serve as the values of the @rhombus(id)s
 for the next iteration. The values of the whole @rhombus(for) expression
 are the final values of the @rhombus(id)s.

}


@doc(
  fun call_with_values(producer :: Function.of_arity(0),
                       consumer :: Function)
){

 Calls @rhombus(producer) with no arguments, and then calls
 @rhombus(consumer) with the result value(s) from @rhombus(producer).

 Use @rhombus(call_with_values) to dispatch on the number of values that
 are produced by an expression. The @rhombus(match) form cannot make that
 distinction, because it always expects a single result value from its
 initial subexpression.

@examples(
  ~defn:
    fun get_fruit(n :: NonnegInt):
      match n
      | 0: values()
      | 1: "apple"
      | 2: values("apple", "banana")
      | ~else values("apple", n +& " bananas")
    fun
    | show(): println("nothing")
    | show(s): println(s)
    | show(a, b): println(a +& " and " +& b)
  ~repl:
    call_with_values(fun (): get_fruit(0), show)
    call_with_values(fun (): get_fruit(1), show)
    call_with_values(fun (): get_fruit(2), show)
    call_with_values(fun (): get_fruit(3), show)
)

}
