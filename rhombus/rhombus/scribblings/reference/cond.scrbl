#lang rhombus/scribble/manual
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

 Static information is gathered from @rhombus(then_body) and
 @rhombus(else_body) under the same conditions as the right-hand side of
 @rhombus(def) (see @secref(~doc: guide_doc, "static-info-rules")), and the information is
 intersected to determine the static information of the @rhombus(if)
 form.

@examples(
  if #true
  | "yes"
  | "no"

  if 1+2 == 3
  | let yep = "yes"
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
 @rhombus(~else) clause, a run-time exception is thrown.

 Static information is gathered from @rhombus(clause_result_body)s or
 @rhombus(clause_result_expr) under the same conditions as the right-hand
 side of @rhombus(def) (see @secref(~doc: guide_doc, "static-info-rules")), and the
 information is intersected to determine the static information of the
 @rhombus(cond) form.


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
  | println("yes")

  when #false
  | println("no")
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
  | println("yes")

  unless #false
  | println("no")
)

}


@doc(
  bind.macro '$bind when $expr'
  bind.macro '$bind unless $expr'
){

 As binding forms, @rhombus(when, ~bind) and @rhombus(unless, ~bind)
 impose side conditions on a match. These forms match and bind the
 same as the given @rhombus(bind), but only when @rhombus(expr)
 produces a true value for @rhombus(when, ~bind) or @rhombus(#false)
 for @rhombus(unless), and the binding does not match otherwise. The
 bindings created by a match to @rhombus(bind) are available for use
 in @rhombus(expr).

 Because the bindings of @rhombus(bind) must be made available for
 @rhombus(expr), a @rhombus(when, ~bind) or @rhombus(unless, ~bind)
 binding is not compatible for use with @rhombus(let), unless the set
 of names from @rhombus(bind) is empty.

 See also @rhombus(where, ~bind).

@examples(
  ~defn:
    fun classify(v):
      match v
      | n :: Integral when n mod 2 .= 0:
          "even"
      | n :: Integral when n mod 2 .= 1:
          "odd"
      | _ :: Real:
          "other number"
      | ~else:
          "other value"
  ~repl:
    classify(1)
    classify(2)
    classify([1, 2])
)

}
