#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@(def check_eval: make_rhombus_eval())

@examples(
  ~eval: check_eval
  ~hidden:
    import lib("racket/base.rkt").#{error-display-handler}
    #{error-display-handler}(fun(msg, exn): display(msg, current_error_port()))
)

@title{Unit Testing}

@doc(
  ~nonterminal:
    expected_expr: begin expr
    expected_body: begin body
    expected_bind: def bind
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
    #,(epsilon)
  
  grammar expected_result:
    ~is $expected_expr
    ~is: $expected_body; ...
    ~is_a $annot
    ~is_a: $annot
    ~prints_like $expected_expr
    ~prints_like: $expected_body; ...
    ~matches $expected_bind
    ~matches: $expected_bind; ...
    ~raises $expected_expr
    ~raises: $expected_body; ...
    ~completes
){

 Evaluates the @rhombus(body) or @rhombus(expr) form, catching any
 exception that is raised, then determines whether the result or
 exception matches @rhombus(expected_result):

@itemlist(

 @item{In @rhombus(~is) mode, evaluates the @rhombus(expected_expr) or
  @rhombus(expected_body), then checks that the original result is not an
  exception and is equal by @rhombus(==).}

 @item{In @rhombus(~is_a) mode, checks that the original result is not
  an exception and satisfies @rhombus(annot).}

 @item{In @rhombus(~prints_like) mode, evaluates the
  @rhombus(expected_expr) or @rhombus(expected_body), then checks that the
  original result is not an exception and has the same printed form as
  expected result.}

 @item{In @rhombus(~matches) mode, checks that the original result is
  not an exception, that the number of result values matches the number of
  supplied @rhombus(expected_bind)s, and that each value matches the
  corresponding @rhombus(expected_bind).}

 @item{In @rhombus(~raises) mode, obtains one or more strings (as
  multiple values) by evaluating @rhombus(expected_body) or
  @rhombus(expected_expr), then checks that original @rhombus(body) or
  @rhombus(expr) raised an exception and that each string is contained in
  the exception message.}

 @item{In @rhombus(~completes) mode, checks merely that the original
  result is not an exception. A @rhombus(#void) is implicitly added to the
  end of @rhombus(body) or @rhombus(expr), so it could end with a definition.}

)

 If @rhombus(~eval) is present, then @rhombus(body) is quoted to be
 parsed and evaluated using @rhombus(eval) at run time. A typical use of
 @rhombus(~eval) is to test syntax errors.

 Providing multiple @rhombus(expr expected_result) groups in a single
 @rhombus(check) form is the same as providing each group to a separate
 @rhombus(check) form. The @rhombus(~eval) mode is not supported in the
 shorthand forms of @rhombus(check).

 When a test fails, a failure message is printed to the current error
 port, but using the current error display handler when a source location
 is available. No output is printed for a successful test.

@examples(
  ~eval: check_eval
  check 1+1 ~is 2
  check 1+1 ~is 3
  check:
    1+1
    ~is 2
  check:
    1+1 ~is 2
    1+1 ~is 3
  check:
    1+1
    ~is_a Int
  check:
    1+1
    ~is_a String
  check:
    [1+1, 1+2]
    ~matches [_ :: Int, ...]
  check:
    '$(1+1)'
    ~prints_like '2'
  check:
    1+"a"
    ~raises "expected: Number"
  ~error:
    check:
      + // oops: causes syntax error for overall `check` form
      ~raises "infix operator without preceding argument"    
  check:
    ~eval // lets `check` confirm the expected syntax error
    +
    ~raises "infix operator without preceding argument"    
  check:
    ~eval
    +
    ~raises values("+", "infix operator without preceding argument")
)

}


@close_eval(check_eval)
