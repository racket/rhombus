#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/rx open)

@(def check_eval = make_rhombus_eval(~attach: #false))
@examples(
  ~eval: check_eval
  ~hidden:
    import lib("racket/base.rkt").#{error-display-handler}
    #{error-display-handler}(fun (msg, exn):
                               print(msg, ~out: stderr))
    import rhombus/rx open
)

@title{Unit Testing}

@doc(
  ~nonterminal:
    expected_expr: block expr
    expected_body: block body
    expected_values_bind: def values_bind ~defn
    evaluator_expr: block expr
  expr.macro 'check:
                $maybe_eval
                $body
                ...
                $expected_result
                ... ~nonempty'
  expr.macro 'check:
                $expr $expected_result ... ~nonempty
                ...'
  expr.macro 'check $expr $expected_result ... ~nonempty'

  grammar maybe_eval
  | ~eval
  | ~eval: $evaluator_expr
  | #,(epsilon)

  grammar expected_result
  | ~is $expected_expr
  | ~is: $expected_body; ...
  | ~is_now $expected_expr
  | ~is_now: $expected_body; ...
  | ~is_a $annot
  | ~is_a: $annot
  | ~prints_like $expected_expr
  | ~prints_like: $expected_body; ...
  | ~matches $expected_values_bind
  | ~matches: $expected_values_bind
  | ~prints $expected_expr
  | ~prints: $expected_body; ...
  | ~throws $expected_expr
  | ~throws: $expected_body; ...
  | ~completes
){

 Evaluates the @rhombus(body) or @rhombus(expr) form, catching any
 exception that is thrown, then determines whether the result or
 exception matches each @rhombus(expected_result):

@itemlist(

 @item{In @rhombus(~is) mode, evaluates the @rhombus(expected_expr) or
  @rhombus(expected_body), then checks that the original result is not an
  exception and is equal by @rhombus(==).}

 @item{In @rhombus(~is_now) mode, check like @rhombus(~is) mode, but
  using @rhombus(is_now).}

 @item{In @rhombus(~is_a) mode, checks that the original result is not
  an exception and satisfies @rhombus(annot).}

 @item{In @rhombus(~prints_like) mode, evaluates the
  @rhombus(expected_expr) or @rhombus(expected_body), then checks that the
  original result is not an exception and has the same printed form as
  the expected result.}

 @item{In @rhombus(~matches) mode, checks that the original result is
  not an exception, that the number of result values matches the number of
  supplied @nontermref(bind)s in @rhombus(expected_values_bind), and that
  each value matches the corresponding @nontermref(bind).}

 @item{In @rhombus(~prints) mode, evaluates the
  @rhombus(expected_expr) or @rhombus(expected_body) to obtain any number
  (as multiple values) of strings or @tech{regexps}, then checks that there is
  no exception and the output string contains each expected string and
  matches each expected regexp. Use @rhombus(rx'') to check that nothing is
  printed, since empty output includes the string @rhombus("").}

 @item{In @rhombus(~throws) mode, evaluates the @rhombus(expected_expr)
  or @rhombus(expected_body) to obtain any number (as multiple values) of
  strings and @tech{regexps}, then checks that original @rhombus(body) or
  @rhombus(expr) threw an exception whose message constains each expected
  string and matches each expected regexp.}

 @item{In @rhombus(~completes) mode, checks merely that the original
  result is not an exception. A @rhombus(#void) is implicitly added to the
  end of @rhombus(body) or @rhombus(expr), so it could end with a definition,
  as long as no other kind of @rhombus(expected_result) is present for
  the same @rhombus(expr) or @rhombus(body) sequence.}

)

 When @rhombus(expected_result)s are present for one @rhombus(expr) or
 @rhombus(body) sequence, then they must all match for the check to be
 considered passing. Printed output is captured for an @rhombus(expr) or
 @rhombus(body) sequence only when at least one @rhombus(expected_result)
 is a @rhombus(~prints_like) or @rhombus(~prints) form.

 If @rhombus(~eval) is present, then @rhombus(body) is quoted to be
 parsed and evaluated using @rhombus(eval) at run time. A typical use of
 @rhombus(~eval) is to test syntax errors. When @rhombus(evaluator_expr)
 is provided, then its result is installed as the current evaluator for
 @rhombus(eval); otherwise, an evaluator is created with
 @rhombus(Evaluator.make_rhombus). The @rhombus(body) sequence is
 evaluated as an interactive form (i.e., @rhombus(eval) is called with
 @rhombus(~as_interactive: #true)).

 Providing multiple @rhombus(expr expected_result ...) groups in a single
 @rhombus(check) form is the same as providing each group to a separate
 @rhombus(check) form. The @rhombus(~eval) mode is not supported in the
 shorthand forms of @rhombus(check).

 The evaluation of an @rhombus(expr) or @rhombus(body) sequence is
 wrapped with a prompt for the default continuation prompt tag.

 When a test fails, a failure message is printed to the current error
 port, but using the current error display handler when a source location
 is available. No output is printed for a successful test.

@examples(
  ~eval: check_eval
  check 1+1 ~is 2
  check 1+1 ~is 3
  check 1+1 ~is_a Int ~prints rx''
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
    1+1
    ~is_a Int
    ~prints rx''
  check:
    [1+1, 1+2]
    ~matches [_ :: Int, ...]
  check:
    '$(1+1)'
    ~prints_like '2'
  check:
    println("hi", "there")
    ~prints values("hi", "ere", rx'["a" - "z" space]*')
  check:
    1+"a"
    ~throws "expected: Number"
  ~error:
    check:
      + // oops: causes syntax error for overall `check` form
      ~throws "infix operator without preceding argument"
  check:
    ~eval // lets `check` confirm the expected syntax error
    +
    ~throws "infix operator without preceding argument"
  check:
    ~eval
    +
    ~throws values("+", "infix operator without preceding argument")
)

}


@(close_eval(check_eval))
