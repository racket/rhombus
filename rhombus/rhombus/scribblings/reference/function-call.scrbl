#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots_expr = @rhombus(...))

@title(~tag: "ref-function-call"){Function Calls}

An expression followed by a parenthesized sequence of expressions is
parsed as an implicit use of the @rhombus(#%call) form, which is
normally bound to implement @tech{function} calls.

@doc(
  ~nonterminal:
    id: block
    fun_expr: block expr
    arg_expr: block expr
    repet_arg: #%call arg
    list_expr: block expr
    map_expr: block expr

  expr.macro '$fun_expr #%call ($arg, ...)'
  repet.macro '$fun_expr #%call ($repet_arg, ...)'
  expr.macro '$fun_expr #%call ($arg, ..., _, $arg, ...)'

  grammar arg:
    $arg_expr
    $keyword
    $keyword: $arg_expr
    $keyword: $body; ...
    $repet #,(@litchar{,}) $ellipses
    & $list_expr
    ~& $map_expr
  grammar ellipses:
    $ellipsis
    $ellipses #,(@litchar{,}) $ellipsis
  grammar ellipsis:
    #,(dots_expr)
){

  A function call. Each @rhombus(arg_expr) alone is a by-position
  argument, and each @rhombus(keyword: arg_expr) combination is a
  by-keyword argument. Function calls can serve as repetitions,
  where @rhombus(repet_arg) is like @rhombus(arg), but with repetitions
  in place of expressions.

  If the @rhombus(arg) sequence contains @rhombus(& list_expr) or
  @rhombus(repet #,(@litchar{,}) ellipses), then the
  elements of the list or @tech{repetition} are spliced into the
  call as separate by-position arguments.

  If the @rhombus(arg) sequence contains @rhombus(~& map_expr), then all
  the keys in the immutable map produced by @rhombus(map_expr) must be keywords, and
  they must not overlap with the directly supplied @rhombus(keyword)s or
  keywords in maps from other @rhombus(~& map_expr) arguments. The
  keyword-value pairs are passed into the function as additional keyword
  arguments.

 Parallel to @rhombus(fun), a single @rhombus(keyword) as an
 @rhombus(arg) is equivalent to the form @rhombus(keyword: id)
 @margin_note{This kind of double duty of a single @rhombus(keyword)
 is sometimes referred to as ``punning.''}
 where @rhombus(id) is composed from @rhombus(keyword) by taking its
 string form and lexical context.

 The case with an immediate @rhombus(_) group among other
 @rhombus(arg)s is a special case for a function shorthand, and it takes
 precedence over parsing the @rhombus(_) as an @rhombus(arg). See
 @rhombus(_) for more information.

 See also @rhombus(use_static).

 @see_implicit(@rhombus(#%call), @parens, "expression", ~is_infix: #true)

@examples(
  List.length([1, 2, 3])
  List.length #%call ([1, 2, 3])
)

}


@doc(
  ~nonterminal:
    arg_expr: block expr
    fun_expr: block expr
  expr.macro '$arg_expr |> $immediate_callee'
  expr.macro '$arg_expr |> $fun_expr'
  operator_order:
    ~order: pipeline
){

 The @rhombus(|>) operator applies its second argument as a function to
 its first argument. That is, @rhombus(arg_expr |> fun_expr) is
 equivalent to @rhombus(fun_expr(arg_expr)), except that
 @rhombus(arg_expr) is evaluated before @rhombus(fun_expr), following
 the textual order.
 @margin_note{This form is known as a ``pipeline.'' Accordingly,
 @rhombus(|>) is the ``pipe-forward'' operator.}

 The left-handle side of @rhombus(|>) is always parsed as an expression,
 @rhombus(arg_expr). The right-hand side of @rhombus(|>) is parsed as an
 @tech(~doc: meta_doc){immediate callee} if possible, propagating of
 static information from @rhombus(arg_expr) in that case, or else the
 right-hand side is parsed as an expression.

 A @rhombus(_) function shorthand can be especially useful with
 @rhombus(|>).

@examples(
  [1, 2, 3] |> List.length
  [3, 1, 2]
    |> List.sort(_)
    |> ([0] ++ _ ++ [100])
    |> to_string.map(_)
)

}

@doc(
  ~nonterminal:
    arg_expr: block expr
    fun_expr: block expr
  expr.macro '$arg_expr ?> $immediate_callee'
  expr.macro '$arg_expr ?> $fun_expr'
  operator_order:
    ~order: pipeline
){

 Like @rhombus(|>), but if the result of @rhombus(arg_expr) is
 @rhombus(#false), the result of the @rhombus(?>) expression is
 @rhombus(#false). Static information is propagated in a way analogous to
 @rhombus(?.).

@examples(
  #false ?> List.length
)

}
