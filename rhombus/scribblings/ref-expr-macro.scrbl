#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm"
    for_label:
      rhombus/macro:
        only: expr_ct
        for_meta -1)

@(val macro_eval: macro.make_macro_eval())

@title{Expression Macros}

@doc(
  defn.macro '«expr.rule $rule_pattern:
                 '$template'»',
  defn.macro '«expr.rule
                | $rule_pattern:
                    '$template'
                | ...»',
  grammar rule_pattern:
    '$identifier_or_operator $pattern ...'
    '$ $term_pattern $identifier_or_operator $pattern ...',
  grammar identifier_or_operator:
    $identifier
    $operator,
  grammar term_pattern:
    $term_identifier
    ($term_identifier :: $syntax_class)
){

 Defines @rhombus(identifier) or @rhombus(operator) as a pattern-based
 macro whose expansion is described by a @rhombus(template) that can
 refer to pattern variables bound in the @rhombus(rule_pattern). A
 @rhombus(rule_pattern) is matched to a portion of its enclosing group,
 and need not extend to the end of the group to match.

 Each @rhombus(rule_pattern) starts with @rhombus(''). Within
 @rhombus(''), either the first term is an identifier or operator to be
 defined as a prefix macro, or a single @rhombus($, ~bind) escape is
 followed by the identifier or operator to be defined as an infix macro.

 Using @litchar{|} alternatives, a single definition can have any number
 of prefix and infix variants (presumably) distinguished by patterns that
 are tried in order.

 The body after each @rhombus(rule_pattern) must be an immediate
 @rhombus('') template, and any @rhombus($) escape within the template can
 only refer to an input pattern variable. More general compile-time
 expressions are not allowed, and @rhombus(expr.macro) must be used
 instead.

@examples(
  ~eval: macro_eval,
  expr.rule 'thunk: $body':
    'fun (): $body',
  thunk: "ok",
  (thunk: "ok")()
)

}


@doc(
  defn.macro 'expr.macro $rule_pattern:
                $body
                ...',
  defn.macro 'expr.macro
              | $rule_pattern:
                  $body
                  ...
              | ...',
){

 Like @rhombus(expr.rule), but

@itemlist(
  
  @item{with arbitrary compile-time code in the body after each
   @rhombus(rule_pattern),},
  
  @item{each @rhombus(rule_pattern) is matched to the entire remainder
   of a group where the macro is used; and},

  @item{a body returns value values: an expansion for the consumed part
   of the input group, and a tail for the unconsumed part.}

)

}

@doc(
  val expr_ct.call_result_key
){

 Provided @rhombus(for_meta, ~impmod), a value that can be used to
 associate static information with an expression that describes the
 result value if the expression is used as a function to call.

}


@«macro.close_eval»(macro_eval)
