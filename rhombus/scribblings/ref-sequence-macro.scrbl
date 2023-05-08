#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Sequence Macros}

@doc(
  ~nonterminal:
    outer_expr: block expr
    loop_id: block id
    loop_expr: block expr
    inner_expr: block expr
    outer_check_expr: block expr
    pos_guard_expr: block expr
    pre_guard_expr: block expr
    post_guard_expr: block expr
    loop_arg_expr: block expr
    loop_body_expr: block expr
    done_expr: block expr

  class_clause.macro '«sequence '$pattern':
                         $body
                         ...»'
  interface_clause.macro '«sequence '$pattern':
                             $body
                             ...»'
){

 Forms for @rhombus(class) or @rhombus(interface) to supply an
 optimizing conversion in an @rhombus(each, ~for_clause) clause of @rhombus(for).
 The conversion applies whenever the right-hand side of
 @rhombus(each, ~for_clause) has static information for the class or
 interface containing the @rhombus(sequence, ~class_clause) declaration.

 The input @rhombus(pattern) is matched to @rhombus(id ...: expr) where
 @rhombus(expr) is a @tech{parsed} expression for the right-hand side of
 @rhombus(each, ~for_clause), and the number of @rhombus(id)s reflects
 the number of expected result values. The result of the @rhombus(body)
 sequence must be either @rhombus(#false), in which case the
 implementation falls back to dynamic mode, or a syntax object of the
 form

@rhombusblock(
  '((#,(@rhombus(outer_ids, ~var)): outer_expr, ...),
    outer_check_expr,
    (loop_id = loop_expr, ...),
    pos_guard_expr,
    (#,(@rhombus(inner_ids, ~var)): inner_expr, ...),
    pre_guard_expr,
    post_guard_expr,
    (loop_arg_expr, ...))'
)

 where each @rhombus(inner_ids, ~var) and @rhombus(outer_ids, ~var) is
 either an identifier or a parenthsized sequence of comma-separated
 identifiers.

 These pieces are integrated into a larger expression of roughly the
 following form, where @rhombus(loop_body_expr) is an iteration body:

@rhombusblock(
  def #,(@rhombus(outer_ids, ~var)): outer_expr
  ...
  fun loop(loop_id = block: loop_expr):
    if pos_guard_expr
    | def #,(@rhombus(inner_ids, ~var)) = inner_expr
      ...
      if pre_guard_expr
      | loop_body_expr
        if post_guard_expr
        | loop(loop_arg_expr, ...)
        | done_expr
      | done_expr
    | done_expr
  loop()
)

 Typically, @rhombus(inner_ids, ~var) includes the the @rhombus(id)s
 supplied to the match for @rhombus(pattern), so they can be referenced
 in @rhombus(loop_body_expr).

@examples(
  ~eval: macro_eval
  ~defn:
    class Posn(x, y):
      implements Sequenceable
      override method to_sequence():
        Sequence.make(~initial_position: 0,
                      ~continue_at_position: fun (i): i < 2,
                      ~position_to_next: fun (i): i + 1,
                      ~position_to_element: fun (i):
                                              if i == 0 | y | x)
      sequence '$lhs: $rhs':
        '((p = $rhs),
          #true,
          (pos = 0),
          pos < 2,
          ($lhs = if pos == 0 | Posn.y(p) | Posn.x(p)),
          #true,
          #true,
          (pos + 1))'
  ~repl:
    for List:
      each j: dynamic(Posn(1, 2)) // uses `to_sequence`
      j
    for List:
      each j: Posn(1, 2) // uses `sequence` expansion
      j
)

}

@«macro.close_eval»(macro_eval)
