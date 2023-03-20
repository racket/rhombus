#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Conditionals}

@doc(
  ~nonterminal:
    test_expr: block expr
    then_body: block body
    else_body: block body
  expr.macro 'if $test_expr
              | $then_body
                ...
              | $else_body
                ...'
){

 If @rhombus(test_expr) produces a true value (which is value other than
 @rhombus(#false)), returns the result of the @rhombus(then_body) clause,
 otherwise returns the result of the @rhombus(else_body) clause.

@examples(
  if #true
  | "yes"
  | "no"

  if 1+2 == 3
  | def yep: "yes"
    yep
  | "no"
)

}

@doc(
  ~nonterminal:
    clause_test_expr: block expr
    clause_result_body: block body
    clause_result_expr: block expr
  expr.macro 'cond
              | $clause_test_expr:
                  $clause_result_body
                  ...
              | ...'
  expr.macro 'cond
              | $clause_test_expr:
                  $clause_result_body
                  ...
              | ...
              | ~else:
                  $clause_result_body
                  ...'
  expr.macro 'cond
              | $clause_test_expr:
                  $clause_result_body
                  ...
              | ...
              | ~else $clause_result_expr'
){

 Tries the @rhombus(clause_test_expr)s in sequence, and as soon as one
 produces a non-@rhombus(#false) value, returns the result of the
 corresponding @rhombus(clause_result_body) block. The keyword
 @rhombus(~else) can be used as a synonym for @rhombus(#true) in the last
 clause.

 If no @rhombus(clause_test_expr) produces a true value and there is no
 @rhombus(~else) clause, a run-time exception is raised.

}

@doc(
  ~nonterminal:
    test_expr: block expr
  expr.macro 'when $test_expr
              | $body
                ...'
){

 If @rhombus(test_expr) produces a true value (which is value other than
 @rhombus(#false)), returns the result of the @rhombus(body) clause,
 otherwise returns @rhombus(#void).

@examples(
  when #true
  | displayln("yes")

  when #false
  | displayln("no")
)

}

@doc(
  ~nonterminal:
    test_expr: block expr
  expr.macro 'unless $test_expr
              | $body
                ...'
){

 If @rhombus(test_expr) produces @rhombus(#false), returns the result
 of the @rhombus(body) clause, otherwise returns @rhombus(#void).

@examples(
  unless #true
  | displayln("yes")
  
  unless #false
  | displayln("no")
)

}
