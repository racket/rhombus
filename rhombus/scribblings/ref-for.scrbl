#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Iteration}

@doc(
  decl.macro 'for:
                $clause_or_body
                ...
                $body',
  decl.macro 'for $reducer:
                $clause_or_body
                ...
                $body',
  decl.macro 'for:
                $clause_or_body
                ...
                $body
                ~into $reducer',
  grammar clause_or_body:
    ~each:
      $binding:
        $body
        ...
      ...
    ~each $binding:
      $body
      ...
    ~when $expr
    ~unless $expr
    ~break_when $expr
    ~final_when $expr
    ~do $clause
    $body
){

 Iterates as determined by @rhombus(~each) clauses among the
 @rhombus(clause_or_body)s. An @rhombus(~each) clause forms one layer
 of iteration; each subsequent part of the body is evaluated once per
 iteration, and additional @rhombus(~each) groups form nested
 iterations.

 The block after a binding within an @rhombus(~each) clause must produce
 a @tech{sequence}. Each element of that sequence is bound in turn to
 the @rhombus(binding) variables of the @rhombus(~each). If a sequence
 can has multiple values as its elements (for example, a map as a
 sequence has a key and value for each element), then
 @rhombus(binding) can be a @rhombus(values, ~bind) pattern or just a
 prenthesized sequence of bindings to receive a matcging number of
 element values.

 An @rhombus(~each) followed immediately by a sequence binding is
 equivalent to @rhombus(~each) followed immediately by a block that
 contains the sequence binding. When multiple sequences are used in
 one iteration layer (i.e., in a block immediately after
 @rhombus(~each)), iteration stops as soon as one of the sequences in
 the layer is exhausted.

 A @rhombus(~when) or @rhombus(~unless) clause short-circuits one
 iteration of the @rhombus(for) body, skipping remining expressions
 (including nested iterations) when the subsequent @rhombus(expr)
 produces @rhombus(#false) for @rhombus(~when) or a true value for
 @rhombus(~unless). A @rhombus(~break_when) clause short-circuits the current
 iteration and all would-be remaining iterations of the @rhombus(for)
 form when its @rhombus(expr) produces a true value. A @rhombus(~final_when)
 clause is similar to @rhombus(~when), but it allows one iteration to
 complete before skipping the reminaing iterations.

 A @rhombus(~do) clause supports extensibility by expanding
 @rhombus(clause) into a sequence that is spliced in place of the
 @rhombus(~do) clause. The predefined clause forms
 @rhombus(each, ~for_clause), @rhombus(when, ~for_clause),
 @rhombus(unless, ~for_clause), @rhombus(break_when, ~for_clause), and
 @rhombus(final_when, ~for_clause) expand to the corresponding keyword
 forms @rhombus(~each), @rhombus(~when), @rhombus(~unless),
 @rhombus(~break_when), and @rhombus(~final_when), so that it's possible
 to write all clauses using @rhombus(~do), and the direct keyword forms
 can be viewed as a shorthand.

 If a @rhombus(reducer) is specified, either before the block or at the
 end with @rhombus(~into), it determines how each result of the last
 @rhombus(body) is combined to produce a result of the overall
 @rhombus(for) expression, otherwise the result of the last
 @rhombus(body) is ignored and the @rhombus(for) expression's value is
 @rhombus(#void). Example reducers include @rhombus(List, ~reducer),
 @rhombus(Map, ~reducer), and @rhombus(values, ~reducer).

@examples(
  for:
    ~each v: ["a", "b", "c"]
    displayln(v),
  for:
    ~each v: ["a", "b", "c"]
    ~when v == "b"
    displayln(v),
  for:
    ~each v: ["a", "b", "c"]
    ~unless v == "b"
    displayln(v),
  for:
    ~each v: ["a", "b", "c"]
    ~break_when v == "b"
    displayln(v),
  for:
    ~each v: ["a", "b", "c"]
    ~final_when v == "b"
    displayln(v),
  for:
    ~each:
      v: ["a", "b", "c"]
      i: 0..
    displayln(i +& ". " +& v),
  fun grid(m, n):
    for List:
      ~each i: 0..m
      ~each j: 0..n
      [i, j],
  grid(2, 3),
  fun sum(l :: List):
    for values(sum = 0):
      ~each i: l
      sum+i,
  sum([2, 3, 4]),
  for:
    ~each i: [1, 2, 3]
    ~each j: 10..10+3
    [i, j]
    ~into List,
  for values(x = 0, y = 2):
    ~each j: 0..3
    values(x + y, j),
  fun grid2(m, n):
    for List:
      ~each i: 0..m
      val k: i + 1
      ~each j: 0..n
      [k, j],
  grid2(2, 3),
  for Map:
    ~each i: 0..3
    values(i, i +& "!")
)

}


@doc(
  expr.macro '$n_expr .. $m_expr',
  expr.macro '$n_expr ..'
){

 If @rhombus(n_expr) produces an integer @rhombus(n, ~var) and
 @rhombus(m_expr) (when supplied) produces an integer @rhombus(m, ~var),
 returns a sequence containing the integers from @rhombus(n, ~var)
 (inclusive) to @rhombus(m, ~var) (exclusive). If @rhombus(m_expr) is not
 specified, the result is an infinite sequence that contains all integers
 starting from @rhombus(n, ~var).

 The @rhombus(..)'s precedence is lower than the arithmetic operators
 @rhombus(+), @rhombus(-), @rhombus(*), and @rhombus(/). In particular,
 @rhombus(n_expr..1+m_expr) creates a sequence that includes
 @rhombus(m, ~var) for many forms @rhombus(m_expr).

}

@doc(
  for_clause.macro 'each:
                      $binding:
                        $body
                        ...
                      ...',
  for_clause.macro 'each $binding:
                      $body
                      ...',
  for_clause.macro 'when $expr',
  for_clause.macro 'unless $expr',
  for_clause.macro 'break_when $expr',
  for_clause.macro 'final_when $expr'
){

 For use with @rhombus(~do) in @rhombus(for), expands to the
 corresponding keyword form: @rhombus(~each), @rhombus(~when),
 @rhombus(~unless), @rhombus(~break_when), or @rhombus(~final_when).

@examples(
  for:
    ~do each v: ["a", "b", "c"]
    ~do unless v == "b"
    displayln(v),
)

}
