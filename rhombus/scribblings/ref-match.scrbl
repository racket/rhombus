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


@doc(
  ~nonterminal:
    left_bind: def bind ~defn
    right_bind: def bind ~defn

  bind.macro '$left_bind where $right_bind = $expr'
  bind.macro '$left_bind where:
                $right_bind_and_expr
                ...'

  grammar right_bind_and_expr:
    $right_bind = $expr
    $right_bind:
      $body
      ...
){

 Creates a binding that matches only when both @rhombus(left_bind)
 matches and when each @rhombus(right_bind) matches the value of the
 corresponding @rhombus(expr) or @rhombus(body) sequence. Names bound by
 @rhombus(left_bind) and each @rhombus(right_bind) are visible in
 @rhombus(expr)s and @rhombus(body) sequences for subsequent
 @rhombus(right_bind)s.

 The @rhombus(where) form should generally be used with parentheses
 around it, since parsing is otherwise likely to interact badly with the
 enclosing context, such as conflicting interpretations of a @rhombus(=)
 by @rhombus(where) and @rhombus(def).

@examples(
  ~repl:
    def ([x, y, z] where sum = x+y+z) = [1, 2, 3]
    '$x $y $z'
    sum
  ~defn:
    fun | f(([x, y, z] where sum = x+y+z) when sum == 6):
            '$x $y $z = $sum'
        | f(_):
            "something else"
  ~repl:
    f([1, 2, 3])
    f([0, 2, 3])
  ~repl:
    def ([x, y, z] where:
           partial_sum = x+y
           sum = partial_sum+z):
      [1, 2, 3]
    sum
  ~repl:
    def ([xs] where [x, ...] = xs): [[1, 2, 3]]
    '$x ...'
  ~repl:
    ~error:
      def ([xs] where [x, ...] = xs): ["oops"]
)

}


@doc(
  expr.macro 'where'
  expr.macro '$left_bind where'
){

 As an expression operator, @rhombus(where) always reports a syntax
 error.

}
