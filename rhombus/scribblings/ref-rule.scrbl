#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title{Syntax Rules}

@doc(
  defn.macro '«rule $rule_pattern:
                 $option; ...
                 '$template'»',
  defn.macro '«rule
               | $rule_pattern:
                   $option; ...
                   '$template'
               | ...»',

  callable.macro '«rule $rule_pattern:
                     $option; ...
                     '$template'»',
  callable.macro '«rule
                   | $rule_pattern:
                       $option; ...
                       '$template'
                   | ...»'
){

 An expression or @tech{callable} shorthand for a function that expects a
 syntax object and returns two syntax-object values. A
 @rhombus(rule_pattern) or @rhombus(template) in @rhombus(rule) is
 the same as in @rhombus(expr.rule) for a prefix operator, but the only
 allowed @rhombus(option) is an @rhombus(~op_stx) form.

 For the purpose of matching a syntax object passed to the function
 produced by @rhombus(rule), the leading identifer in each
 @rhombus(rule_template) pattern is replaced by @rhombus(_, ~bind), and a
 @rhombus($tail ..., ~bind) pattern is added to the end. The result
 values are the syntax object produced by @rhombus(template) and
 @rhombus('$tail ...').

}
