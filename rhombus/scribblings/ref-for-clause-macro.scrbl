#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{For Clause Macros}

@doc(
  defn.macro 'for_clause.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'for_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Defines an @rhombus(identifier, ~var), @rhombus(operator, ~var),
 @rhombus(identifier_path, ~var), or @rhombus(operator_path, ~var) as a
 @rhombus(for) clause form, where @rhombus(rule_pattern) and form is the
 same as for @rhombus(defn.macro). The macro pattern is matched to an
 entire group in a definition context.

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use, where each group can be either a another @rhombus(for) clause, an
 expression, or a defintion.

@examples(
  ~eval: macro_eval
  for_clause.macro 'each_in_three $id':
    'def three: 3
     each $id: 0..three'
  for List:
    each_in_three i
    i
)

}

@«macro.close_eval»(macro_eval)
