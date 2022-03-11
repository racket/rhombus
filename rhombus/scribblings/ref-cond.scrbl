#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Conditionals}

@doc[
  decl.macro 'if $test_expr
              | $then_body
                ...
              | $else_body
                ...'
]{

 If @rhombus[test_expr] produces a true value (which is value other than
 @rhombus[#false]), returns the result of the @rhombus[then_body] clause,
 otherwise returns the result of the @rhombus[else_body] clause.

@examples[
  if #true
  | "yes"
  | "no",
  if 1+2 == 3
  | val yep: "yes"
    yep
  | "no"
]

}

@doc[
  decl.macro 'cond
              | $clause_test_expr:
                  $clause_result_body
                  ...
              | ...',
  decl.macro 'cond
              | $clause_test_expr:
                  $clause_result_body
                  ...
              | ...
              | ~else:
                  $clause_result_body
                  ...'
]{

 Tries the @rhombus[clause_test_expr]s in sequence, and as soon as one
 produces a non-@rhombus[#false] value, returns the result of the
 corresponding @rhombus[clause_result_body] block. The keyword
 @rhombus[~else] can be used as a synonym for @rhombus[#true] in the last
 clause.

 If no @rhombus[clause_test_expr] produces a true value and there is no
 @rhombus[~else] clause, a run-time exception is raised.

}
