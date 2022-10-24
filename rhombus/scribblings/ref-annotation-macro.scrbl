#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(val macro_eval: macro.make_macro_eval())

@(val dollar: @rhombus($))

@title{Annotation Macros}

@doc(
  defn.macro '«annotation.rule $rule_pattern:
                 $option; ...
                 '$template'»',
  defn.macro '«annotation.rule
               | $rule_pattern:
                   $option; ...
                   '$template'
               | ...»'
){

 Defines @rhombus(identifier) or @rhombus(operator) as an annotation form,
 which is implemented pattern-based macro whose expansion is described
 by a @rhombus(template) that can refer to pattern variables bound in
 the @rhombus(rule_pattern). The @rhombus(rule_pattern) and
 @rhombus(template) forms are the same as for @rhombus(expr.rule).

@examples(
  ~eval: macro_eval,
  annotation.rule 'two_of($ann)':
    'matching(List(_ :: $ann, _ :: $ann))',
  [1, 2] :: two_of(Number),
  ~error: [1, 2, 3] :: two_of(Number),
  ~error: [1, "x"] :: two_of(Number)
)

}


@doc(
  defn.macro 'annotation.macro $rule_pattern:
                $option; ...
                $body
                ...',
  defn.macro 'annotation.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...',
){

 Like @rhombus(expr.macro), but producing an annotation like
 @rhombus(annotation.rule), instead of an expression.
 
}

@doc(
  fun annotation_meta.pack_predicate(fun_stx:: Syntax,
                                     statinfo_stx :: Syntax = '()') :: Syntax
){

 @provided_meta()

 Packs an expression for a predicate with static information into an
 annotation form as a syntax object. When the resulting annotation is
 applied to a value, it checks the value using the predicate, and it
 also associates the static information in @rhombus(statinfo_stx) with
 the value. The given @rhombus(statinfo_stx) is in unpacked form
 (i.e., @rhombus(statinfo_meta.pack) is applied automatically).

 See @secref("annotation-macro") for more explanation and for
 examples.

}


@«macro.close_eval»(macro_eval)
