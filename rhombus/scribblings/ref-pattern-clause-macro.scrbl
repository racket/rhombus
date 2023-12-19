#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

@title{Syntax Pattern Clause Macros}

@doc(
  ~nonterminal:
    pattern_body: syntax_class ~defn

  space.transform pattern_clause
){

 The @tech{space} for bindings of identifiers that implement
 @rhombus(pattern_body) clauses.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
    pattern_body: syntax_class ~defn
    body: block

  defn.macro 'pattern_clause.macro $prefix_macro_patterns'
){

 Similar to @rhombus(defn.macro), but defines a name in the
 @rhombus(pattern_clause, ~space) @tech{space} as a clause form
 for use within a @rhombus(pattern_body).

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use within a @rhombus(pattern_body).

}


@(macro.close_eval(macro_eval))
