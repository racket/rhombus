#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title(~tag: "ref-recur"){Recursion}

Rhombus functions defined with @rhombus(fun) can be recursive, of
course, but the @rhombus(recur) form offers a shorthand for the case
that a recursive function is used just once and is better written
inline. Iteration with @rhombus(for) is also a kind of recursion, but
@rhombus(recur) supports non-tail recursion, instead of only iterative
loops.

@doc(
  ~nonterminal:
    bind_maybe_kw_opt: fun ~defn
    maybe_res_annot: fun ~defn

  expr.macro 'recur $id($bind_maybe_kw_opt, ...) maybe_res_annot:
                $body
                ...'
){

 Similar to defining @rhombus(id) as a function and immediately calling
 it, where @rhombus(id) is bound only within the @rhombus(body) block for
 recursive calls. The intent is to implement a recursive calculation ``in
 place,'' instead of defining a recursive function and then calling
 it---including cases where the recursion is not in tail position or
 where @rhombus(for) is not a good fit for some other reason.

 @margin_note_block{Racket and Scheme programmers will recognize this form as
  a kind of ``@as_index{named @tt{let}}.''}

 To enable the immediate call of @rhombus(id), each
 @rhombus(bind_maybe_kw_opt) must include a ``default'' expression, and
 the immediate call is like supplying zero arguments. Thus, each
 ``default'' expression is really an initial-value expression. The
 binding of @rhombus(id) within the @rhombus(body) block, however, is a
 function where only keyword arguments are optional. The default value
 for a keyword argument in a recursive call, meanwhile, is whatever was
 supplied for the enclosing call, with the effect that keyword arguments
 are automatically propagated in a recursive call. That treatment of keyword
 arguments is different from a @rhombus(fun, ~defn)-defined function that
 calls itself.

 Beware that when @rhombus(maybe_res_annot) specifies a result
 annotation using @rhombus(::, ~bind), the annotation is checked on every
 recursive call. In that case, no recursive calls are in tail position.

@examples(
  ~repl:
    recur nest_to(n = 5):
      match n
      | 0: [0]
      | ~else: [n, nest_to(n-1)]
  ~repl:
    recur sum_halves(l = [1, 2, 3, -4, 5, -6],
                     ~pos_sum: pos_sum = 0,
                     ~neg_sum: neg_sum = 0):
      match l
      | []:
          values(pos_sum, neg_sum)
      | [n, & ns]:
          if n > 0
          | sum_halves(ns, ~pos_sum: pos_sum + n)
          | sum_halves(ns, ~neg_sum: neg_sum + n)
)

}
