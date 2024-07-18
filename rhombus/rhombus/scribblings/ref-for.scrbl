#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Iteration}

@doc(
  expr.macro 'for $maybe_each:
                $clause_or_body
                ...
                $body'
  expr.macro 'for $reducer $maybe_each:
                $clause_or_body
                ...
                $body'
  expr.macro 'for $maybe_each:
                $clause_or_body
                ...
                $body
                ~into $reducer'
  grammar maybe_each:
    ($bind:
       $body
       ...,
     ...)
    #,(epsilon)
  grammar clause_or_body:
    #,(@rhombus(each, ~for_clause)) $bind:
      $body
      ...
    #,(@rhombus(each, ~for_clause)):
      $bind:
        $body
        ...
      ...
    #,(@rhombus(keep_when, ~for_clause)) $expr_or_block
    #,(@rhombus(skip_when, ~for_clause)) $expr_or_block
    #,(@rhombus(break_when, ~for_clause)) $expr_or_block
    #,(@rhombus(final_when, ~for_clause)) $expr_or_block
    $other_for_clause
    $body

  grammar expr_or_block:
    $expr
    : $body; ...
){

 Iterates as determined by @rhombus(maybe_each) and @rhombus(each, ~for_clause) clauses among the
 @rhombus(clause_or_body)s, where a non-empty @rhombus(maybe_each) is converted to
 an initial @rhombus(each, ~for_clause) before all @rhombus(clause_or_body)s.
 An @rhombus(each, ~for_clause) clause forms one layer
 of iteration; each subsequent part of the body is evaluated once per
 iteration, and additional @rhombus(each, ~for_clause) groups form nested
 iterations.

 The block after a binding within an @rhombus(each, ~for_clause) clause must produce
 a @tech{sequence}. When @rhombus(for) is static (see
 @rhombus(use_static)), static information for the block must indicate
 a static sequence implementation that may specialize iteration over the
 sequence; see also @rhombus(Sequence, ~annot) and @rhombus(statinfo_meta.sequence_constructor_key).
 A fresh @tech{instantiation} of the sequence is used each
 time the @rhombus(for) form is evaluated. Each element of that sequence is bound in turn to
 the @rhombus(bind) variables of the @rhombus(each, ~for_clause). If a sequence
 can has multiple values as its elements (for example, a map as a
 sequence has a key and value for each element), then
 @rhombus(bind) can be a @rhombus(values, ~bind) pattern or just a
 prenthesized sequence of bindings to receive a matching number of
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
    for (v: ["a", "b", "c"]):
      println(v)
    for:
      each v: ["a", "b", "c"]
      println(v)
    for:
      each v: ["a", "b", "c"]
      keep_when v == "b"
      println(v)
    for:
      each v: ["a", "b", "c"]
      skip_when v == "b"
      println(v)
    for:
      each v: ["a", "b", "c"]
      break_when v == "b"
      println(v)
    for:
      each v: ["a", "b", "c"]
      final_when v == "b"
      println(v)
    for (v: ["a", "b", "c"],
         i: 0..):
      println(i +& ". " +& v)
    for:
      each:
        v: ["a", "b", "c"]
        i: 0..
      println(i +& ". " +& v)
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
        let k = i + 1
        each j: 0..n
        [k, j]
    grid2(2, 3)
  ~repl:
    for Map (i: 0..3):
      values(i, i +& "!")
    for Map:
      each i: 0..3
      values(i, i +& "!")
)

}


@doc(
  for_clause.macro 'each:
                      $bind:
                        $body
                        ...
                      ...'
  for_clause.macro 'each $bind:
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

}
