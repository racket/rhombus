#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title{Syntax Rules}

@doc(
  defn.macro '«rule $rule_pattern:
                 $option; ...
                 '$template'»'
  defn.macro '«rule
               | $rule_pattern:
                   $option; ...
                   '$template'
               | ...»'

  expr.macro '«rule '() $pattern ...':
                 $option; ...
                 '$template'»'
  expr.macro '«rule
               | '() $pattern':
                   $option; ...
                   '$template'
               | ...»'

  entry_point.macro '«rule '() $pattern ...':
                       $option; ...
                       '$template'»'
  entry_point.macro '«rule
                      | '() $pattern ...':
                          $option; ...
                          '$template'
                      | ...»',
  
){

 As a definition form, @rhombus(rule) is a shorthand for
 @rhombus(expr.rule). See @rhombus(expr.rule) for the definition
 of @rhombus(rule_pattern), @rhombus(option), and @rhombus(template).

 As an expression or @tech{entry point}, shorthand for a function that
 expects a syntax object and returns two syntax-object values. The
 @rhombus(()) at the start of the pattern stands for an unspecified
 term---normally an identifier that triggers a use of the rule. A
 @rhombus(template) is the same as in @rhombus(expr.rule) for a prefix
 operator, but the only allowed @rhombus(option) is an @rhombus(~op_stx)
 form.

 For the purpose of matching a syntax object passed to the function
 produced by @rhombus(rule), the leading @rhombus(()) in each pattern is
 replaced by @rhombus(_, ~bind), and a @rhombus($tail ..., ~bind) pattern
 is added to the end. The result values are the syntax object produced by
 @rhombus(template) and @rhombus('$tail ...').

}
