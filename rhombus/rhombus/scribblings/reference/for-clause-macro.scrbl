#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

@title{For Clause Macros}

@doc(
  space.transform for_clause
){

 The @tech{space} for bindings of identifiers that implement
 @rhombus(for) clauses.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
    body: block

  defn.macro 'for_clause.macro $prefix_macro_patterns'
){

 Similar to @rhombus(defn.macro), but defines a name in the
 @rhombus(for_clause, ~space) @tech{space} as a clause form
 for use within a @rhombus(for) body.

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use, where each group can be either a another @rhombus(for) clause, an
 expression, or a definition.

@examples(
  ~eval: macro_eval
  ~defn:
    for_clause.macro 'each_in_three $id':
      'def three: 3
       each $id: 0..three'
  ~repl:
    for List:
      each_in_three i
      i
)

}


@(macro.close_eval(macro_eval))
