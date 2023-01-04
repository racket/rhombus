#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Annotation Macros}

@doc(
  defn.macro '«annot.rule $rule_pattern:
                 $option; ...
                 '$template'»'
  defn.macro '«annot.rule
               | $rule_pattern:
                   $option; ...
                   '$template'
               | ...»'
){

 Defines an @rhombus(identifier, ~var) or @rhombus(operator, ~var) as an annotation form,
 which is implemented pattern-based macro whose expansion is described
 by a @rhombus(template) that can refer to pattern variables bound in
 the @rhombus(rule_pattern). The @rhombus(rule_pattern) and
 @rhombus(template) forms are the same as for @rhombus(expr.rule).

@examples(
  ~eval: macro_eval
  annot.rule 'two_of($ann)':
    'matching(List(_ :: $ann, _ :: $ann))'
  [1, 2] :: two_of(Number)
  ~error: [1, 2, 3] :: two_of(Number)
  ~error: [1, "x"] :: two_of(Number)
)

}


@doc(
  defn.macro 'annot.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'annot.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Like @rhombus(expr.macro), but producing an annotation like
 @rhombus(annot.rule), instead of an expression.
 
}

@doc(
  fun annot_meta.pack_predicate(fun_stx:: Syntax,
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


@doc(
  defn.macro 'annot.only.rule $rule_decl'
  defn.macro 'annot.only.macro $rule_decl'
){

 Like @rhombus(annot.rule) and @rhombus(annot.macro), but the identifier
 or operator is bound only in the @rhombus(rhombus/annot, ~datum) @tech{space}.

}


@«macro.close_eval»(macro_eval)
