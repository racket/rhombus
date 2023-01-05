#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Unit Testing}

@doc(
  expr.macro 'check:
                $maybe_eval
                $body
                ...
                $expected_result'
  expr.macro 'check:
                $expr $expected_result
                ...'
  expr.macro 'check $expr $expected_result'

  grammar maybe_eval:
    ~eval
    $$(epsilon)
  
  grammar expected_result:
    ~is $expected_expr
    ~is:
      $expected_body
      ...
    ~prints_like $expected_expr
    ~prints_like:
      $expected_body
      ...
    ~raises $expected_expr
    ~raises:
      $expected_body
      ...
    ~completes
){

 Evaluates the @rhombus(body) or @rhombus(expr) form, catching any
 exception that is raised, then evaluates the @rhombus(expected_body) or
 @rhombus(expected_expr) form and determines whether the former results
 match the latter. In @rhombus(~is) mode, the two results must be equal
 by @rhombus(==). In @rhombus(~prints_like) mode, the two results must
 have the same printed form. In @rhombus(~raises) mode, the strings
 produced (as multiple values) by @rhombus(expected_body) or
 @rhombus(expected_expr) must be contained in the error message of an
 exception raised by @rhombus(body) or @rhombus(expr). In
 @rhombus(~completes) mode, the @rhombus(body) or @rhombus(expr) can
 produce any values.

 If @rhombus(~eval) is present, then @rhombus(body) is quoted to be
 parsed and evaluated using @rhombus(eval) at run time. A typical use of
 @rhombus(~eval) is to test for syntax errors.

 Providing multiple @rhombus(expr expected_result) groups in a single
 @rhombus(check) form is the same as providing each group to a separate
 @rhombus(check) form. The @rhombus(~eval) mode is not supported in the
 two shorthand forms of @rhombus(check).

 When a test fails, a failure message is printed to the current error
 port. Nothing output is printed for a successful test.

@examples(
  check 1+1 ~is 2
  check 1+1 ~is 3
  check:
    1+1
    ~is 2
  check:
    '$(1+1)'
    ~prints_like '2'
  check:
    1+"a"
    ~raises "expected: Number"
  check:
    ~eval
    +
    ~raises "infix operator without preceding argument"    
)

}
