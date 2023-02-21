#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Matching}

@doc(
  expr.macro 'match $target_expr
              | $binding:
                  $result_body
                  ...
              | ...'
  expr.macro 'match $target_expr
              | $binding:
                  $result_body
                  ...
              | ...
              | ~else:
                  $result_body
                  ...'
  expr.macro 'match $target_expr
              | $binding:
                  $result_body
                  ...
              | ...
              | ~else $result_expr'
){

 Tries matching the result of @rhombus(target_expr) against each
 @rhombus(binding) in sequence, and as soon as one matches, returns the
 result of the corresponding @rhombus(result_body) block. The keyword
 @rhombus(~else) can be used as a synonym for @rhombus(_, ~bind) (which matches
 any value without binding any identifiers) in the last clause.

 Typically, a @rhombus(binding) imposes requires on a value and binds
 some number of identifiers as a result of a successful match. For
 example, a literal number works as a @rhombus(binding) pattern, but it
 binds zero identifiers. An identifier as a @rhombus(binding) pattern
 matches any value and binds the identifier the the matching value. A
 list form is a @rhombus(binding) pattern with subpatterns as its
 elements, and it matches a list with the right number of elements that
 match match the corresponding pattern. The set of @rhombus(binding)
 forms is extensible, so it cannot be completely enumerated here.

 If no @rhombus(target_expr) produces a true value and there is no
 @rhombus(~else) clause, a run-time exception is raised.

@examples(
  match 1+2
  | 3: "three"
  | ~else: "not three"

  match [1+2, 3+4]
  | [x, y]: x+y
)

 The @rhombus(match) form's binding is recognized specially in
 @rhombus(fun), @rhombus(operator), @rhombus(macro) and similar forms to
 group @vbar alternatives while supplying information that applies to all
 alternatives. Those uses of @rhombus(match) do not have a
 @rhombus(target_expr), and they have a different syntax for the
 @vbar clauses inside @rhombus(match).

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
