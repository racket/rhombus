#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Matching}

@doc(
  ~nonterminal:
    target_expr: block expr
    result_body: block body
    result_expr: block expr

  expr.macro 'match $target_expr
              | $bind:
                  $result_body
                  ...
              | ...'
  expr.macro 'match $target_expr
              | $bind:
                  $result_body
                  ...
              | ...
              | ~else:
                  $result_body
                  ...'
  expr.macro 'match $target_expr
              | $bind:
                  $result_body
                  ...
              | ...
              | ~else $result_expr'
){

 Tries matching the result of @rhombus(target_expr) against each
 @rhombus(bind) in sequence, and as soon as one matches, returns the
 result of the corresponding @rhombus(result_body) block. The keyword
 @rhombus(~else) can be used as a synonym for @rhombus(_, ~bind) (which matches
 any value without binding any identifiers) in the last clause.

 Typically, a @rhombus(bind) imposes requires on a value and binds
 some number of identifiers as a result of a successful match. For
 example, a literal number works as a @rhombus(bind) pattern, but it
 binds zero identifiers. An identifier as a @rhombus(bind) pattern
 matches any value and binds the identifier the the matching value. A
 list form is a @rhombus(bind) pattern with subpatterns as its
 elements, and it matches a list with the right number of elements that
 match match the corresponding pattern. A @rhombus(when, ~bind) or
 @rhombus(unless, ~bind) can impose a side condition on a match.
 The set of @rhombus(bind) forms is extensible, so it cannot be
 completely enumerated here.

 If no @rhombus(target_expr) produces a true value and there is no
 @rhombus(~else) clause, a run-time exception is raised. In that case,
 when all of the @rhombus(bind) forms are syntax-object patterns, the
 generated exception's message may be specialized to report the expcted
 pattern, instead of just reporting that no cases matched.

@examples(
  match 1+2
  | 3: "three"
  | ~else: "not three"

  match [1+2, 3+4]
  | [x, y]: x+y

  ~error:
    match 'go ~slow':
    | 'go ~fast': "ok"

  ~error:
    match 1+2:
    | n when n > 4: "ok"
)
}

@doc(
  bind.macro '_'
){

 Matches any value without binding any identifiers.

@examples(
  match 1+2
  | 0: "zero"
  | _: "nonzero"
)

}
