#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Iteration}

@doc(
  decl.macro 'for:
                $clause_or_body
                ...
                $body'
  decl.macro 'for $reducer:
                $clause_or_body
                ...
                $body'
  decl.macro 'for:
                $clause_or_body
                ...
                $body
                ~into $reducer'
  grammar clause_or_body:
    #,(@rhombus(each, ~for_clause)) $binding:
      $body
      ...
    #,(@rhombus(each, ~for_clause)):
      $binding:
        $body
        ...
      ...
    #,(@rhombus(keep_when, ~for_clause)) $expr_or_block
    #,(@rhombus(skip_when, ~for_clause)) $expr_or_block
    #,(@rhombus(break_when, ~for_clause)) $expr_or_block
    #,(@rhombus(final_when, ~for_clause)) $expr_or_block
    $other_for_clause
    $body
){

 Iterates as determined by @rhombus(each, ~for_clause) clauses among the
 @rhombus(clause_or_body)s. An @rhombus(each, ~for_clause) clause forms one layer
 of iteration; each subsequent part of the body is evaluated once per
 iteration, and additional @rhombus(each, ~for_clause) groups form nested
 iterations.

 The block after a binding within an @rhombus(each, ~for_clause) clause must produce
 a @tech{sequence}. Each element of that sequence is bound in turn to
 the @rhombus(binding) variables of the @rhombus(each, ~for_clause). If a sequence
 can has multiple values as its elements (for example, a map as a
 sequence has a key and value for each element), then
 @rhombus(binding) can be a @rhombus(values, ~bind) pattern or just a
 prenthesized sequence of bindings to receive a matcging number of
 element values.

 An @rhombus(each, ~for_clause) followed immediately by a sequence binding is
 equivalent to @rhombus(each, ~for_clause) followed immediately by a block that
 contains the sequence binding. When multiple sequences are used in
 one iteration layer (i.e., in a block immediately after
 @rhombus(each, ~for_clause)), iteration stops as soon as one of the sequences in
 the layer is exhausted.

 A @rhombus(keep_when, ~for_clause) or @rhombus(skip_when, ~for_clause) clause short-circuits one
 iteration of the @rhombus(for) body, skipping remining expressions
 (including nested iterations) when the subsequent @rhombus(expr)
 produces @rhombus(#false) for @rhombus(keep_when, ~for_clause) or a true value for
 @rhombus(skip_when, ~for_clause). A @rhombus(break_when, ~for_clause) clause short-circuits the current
 iteration and all would-be remaining iterations of the @rhombus(for)
 form when its @rhombus(expr) produces a true value. A @rhombus(final_when, ~for_clause)
 clause is similar to @rhombus(keep_when, ~for_clause), but it allows one iteration to
 complete before skipping the reminaing iterations.

 New @rhombus(for) clause forms can be defined as macros that
 (eventually) expand to the core forms that are recognized by
 @rhombus(for).

 If a @rhombus(reducer) is specified, either before the block or at the
 end with @rhombus(~into), it determines how each result of the last
 @rhombus(body) is combined to produce a result of the overall
 @rhombus(for) expression, otherwise the result of the last
 @rhombus(body) is ignored and the @rhombus(for) expression's value is
 @rhombus(#void). Example reducers include @rhombus(List, ~reducer),
 @rhombus(Map, ~reducer), and @rhombus(values, ~reducer).

@examples(
  ~repl:
    for:
      each v: ["a", "b", "c"]
      displayln(v)
    for:
      each v: ["a", "b", "c"]
      keep_when v == "b"
      displayln(v)
    for:
      each v: ["a", "b", "c"]
      skip_when v == "b"
      displayln(v)
    for:
      each v: ["a", "b", "c"]
      break_when v == "b"
      displayln(v)
    for:
      each v: ["a", "b", "c"]
      final_when v == "b"
      displayln(v)
    for:
      each:
        v: ["a", "b", "c"]
        i: 0..
      displayln(i +& ". " +& v)
  ~repl:
    fun grid(m, n):
      for List:
        each i: 0..m
        each j: 0..n
        [i, j]
    grid(2, 3)
  ~repl:
    fun sum(l :: List):
      for values(sum = 0):
        each i: l
        sum+i
    sum([2, 3, 4])
  ~repl:
    for:
      each i: [1, 2, 3]
      each j: 10..10+3
      [i, j]
      ~into List
  ~repl:
    for values(x = 0, y = 2):
      each j: 0..3
      values(x + y, j)
  ~repl:
    fun grid2(m, n):
      for List:
        each i: 0..m
        def k: i + 1
        each j: 0..n
        [k, j]
    grid2(2, 3)
  ~repl:
    for Map:
      each i: 0..3
      values(i, i +& "!")
)

}


@doc(
  expr.macro '$n_expr .. $m_expr'
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
                      ...'
  for_clause.macro 'each $binding:
                      $body
                      ...'
  for_clause.macro 'keep_when $expr'
  for_clause.macro 'keep_when: $body; ...'
  for_clause.macro 'skip_when $expr'
  for_clause.macro 'skip_when: $body; ...'
  for_clause.macro 'break_when $expr'
  for_clause.macro 'break_when: $body; ...'
  for_clause.macro 'final_when $expr'
  for_clause.macro 'final_when: $body; ...'
){

 The primitive clause forms that are recognized by @rhombus(for).

@examples(
  for:
    each v: ["a", "b", "c"]
    skip_when v == "b"
    displayln(v),
)

}
