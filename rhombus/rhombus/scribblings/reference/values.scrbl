#lang rhombus/scribble/manual
@(import:
    "common.rhm"open
    "nonterminal.rhm" open)

@title{Multiple Values}

@doc(
  expr.macro 'values'
  expr.macro 'values(expr, ...)'
  repet.macro 'values'
  repet.macro 'values(repet, ...)'
){

 A @rhombus(values) form by itself acts as a function that takes any
 number of values and returns them as multiple result values. If only one
 @rhombus(expr) is provided, the result is the same as just
 @rhombus(expr), except that it is not in tail position with respect to
 the @rhombus(values) form. Any other number of values must be received
 by a context that is expecting multiple values, such as with a
 @rhombus(values, ~bind) binding pattern.

 When @rhombus(values) is immediately called using the default
 @rhombus(#%call) operator, then static information for the call
 expression propagates static information for the @rhombus(expr)
 arguments (using @rhombus(statinfo_meta.values_key)).

 The @rhombus(values, ~repet) operator can also be used to form a
 @tech{repetition}.

@examples(
  values("apple")
  values("apple", 1)
  block:
    let (x, y) = values(1, 2)
    [x, y]
)

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

 Plain parentheses as a binding (as implemented by the
 @rhombus(#%parens, ~bind) form) work as an alias for
 @rhombus(values, ~bind) in the places that recognize
 @rhombus(values, ~bind).

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
  reducer.macro 'values($id $maybe_annot $init, ...)'
  reducer.macro 'fold($id $maybe_annot $init, ...)'

  grammar maybe_annot:
    #,(@rhombus(::, ~bind)) $annot
    #,(@rhombus(:~, ~bind)) $annot
    #,(epsilon)

  grammar init:
    = $expr
    : $body; ...
){

 A @tech(~doc: guide_doc){reducer} used with @rhombus(for), expects as many results from a
 @rhombus(for) body as @rhombus(id)s. When @rhombus(id) is
 @rhombus(_, ~bind), a fresh identifier is used, otherwise
 @rhombus(id) is bound as follows. For the first iteration of
 the @rhombus(for) body, each @rhombus(id)'s value is the result
 of the corresponding @rhombus(init). The results of a @rhombus(for) body
 for one iteration then serve as the values of the @rhombus(id)s
 for the next iteration. The values of the whole @rhombus(for) expression
 are the final values of the @rhombus(id)s.

 The @rhombus(fold, ~reducer) reducer form is an alias for
 @rhombus(values, ~reducer).

@examples(
  for fold(sum = 0) (i in 1..=10):
    sum + i
  for values(sum = 0, product = 1) (i in 1..=10):
    values(sum + i, product * i )
)

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
      | ~else: values("apple", n +& " bananas")
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
