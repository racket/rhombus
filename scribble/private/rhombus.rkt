#lang rhombus
import:
  rhombus/macro: no_prefix
  "typeset-rhombus.rkt": no_prefix
  scribble/base:
    prefix scribble
    expose: elem

export:
  rhombus
  rhombusblock

expr.macro '(rhombus ($forms ...) $main_tail ......):
  def | map(proc, []): []
      | map(proc, [a, as, ...]): cons(proc(a), map(proc, as))
  def | andmap(proc, []): #true
      | andmap(proc, [a, as, ...]): proc(a) && andmap(proc, as)  
  def head_context(stxs):
    match stxs
    | '($head $_ ......):
        relocate_syntax('#false, head)
  def head_escape_context(stxs):
    match stxs
    | '($head $esc $_ ......):
        relocate_span_syntax('#false, [head, esc])
  def literal(stxs):
    '(literal_syntax $stxs)
  def is_literal(stx):
    match stx
    | '(literal_syntax $_): #true
    | ~else: #false
  def nested(ess, builder, stxs):
    val new_ess: map(fun(es): escape('($es ...)), ess)
    if andmap(is_literal, new_ess)
    | literal(stxs)
    | '($builder([$new_ess, ...], literal_syntax_term($(head_context(stxs)))))
  def escape(stxs):
    match stxs
    | '($$($expr ...) $tail ......):
        '(sequence_cons_syntax(elem($expr ...), $(escape(tail)),
                               literal_syntax_term($(head_escape_context(stxs)))))
    | '($head $next $tail ......):
        val new_head: escape('($head))
        val new_tail: escape('($next $tail ......))
        if is_literal(new_head)  && is_literal(new_tail)
        | literal(stxs)
        | '(sequence_append_syntax($(escape('($head))), $new_tail,
                                   literal_syntax_term($(head_context(stxs)))))
    | '(($ess ..., ...)):
        nested(ess, 'parens_syntax, stxs)
    | '([$ess ..., ...]):
        nested(ess, 'brackets_syntax, stxs)
    | '({$ess ..., ...}):
        nested(ess, 'braces_syntax, stxs)
    | '(: $ess ...; ...):
        nested(ess, 'block_syntax, stxs)
    | ~else: literal(stxs)
  values('(#{typeset-rhombus}($(escape('($forms ...))))),
         main_tail)

fun as_seq(v): '($v)

fun sequence_cons_syntax(a, d, context):
  let a_r: relocate_syntax(to_syntax(a), context);
  match d
  | '($e ......): '($a_r $e ......)

fun sequence_append_syntax(d1, d2, context):
  match d1
  | '($e1 ...):
      match d2
      | '($e2 ......):
          relocate_syntax('($e1 ... $e2 ......), context)
      
fun parens_syntax(gs, context):
  match gs
  | ['($g ...), ...]: as_seq(relocate_syntax('($g ..., ...), context))

fun brackets_syntax(gs, context):
  match gs
  | ['($g ...), ...]: as_seq(relocate_syntax('[$g ..., ...], context))

fun braces_syntax(gs, context):
  match gs
  | ['($g ...), ...]: as_seq(relocate_syntax('{$g ..., ...}, context))

fun block_syntax(gs, context):
  match gs
  | ['($g ...), ...]: as_seq(relocate_syntax(syntax_term(: $g ...; ...), context))

expr.macro '(rhombusblock $tail ...):
  ~op_stx: me
  match '($tail ...)
  | '(: $content ...; ...):
      values('(#{typeset-rhombusblock}(literal_syntax($tail ...))), '())
  | ~else: raise_syntax_error("expected a block", '($me $tail ...))
