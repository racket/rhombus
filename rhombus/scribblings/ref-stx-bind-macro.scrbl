#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Syntax Binding Macros}

@doc(
  defn.macro 'syntax_binding.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'syntax_binding.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Like @rhombus(expr.macro), but for binding an operator that works
 within a @rhombus($, ~bind) escape for a syntax-pattern binding.

@examples(
  ~eval: macro_eval
  syntax_binding.macro 'dots':
    '«'$('...')'»'
  match Syntax.make_group(['...', '...', '...'])
  | '$dots ...': "all dots"
)

}

@«macro.close_eval»(macro_eval)
