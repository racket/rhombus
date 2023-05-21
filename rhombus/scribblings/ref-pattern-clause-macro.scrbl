#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Syntax Pattern Clause Macros}

@doc(
  space.transform pattern_clause
){

 The @tech{space} for bindings of identifiers that implement
 @rhombus(pattern, ~bind) clauses.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro

  defn.macro 'pattern_clause.macro $prefix_macro_patterns'
){

 Similar to @rhombus(defn.macro, ~expr), but defines a name in the
 @rhombus(pattern_clause, ~space) @tech{space} as a clause form
 for use within a @rhombus(pattern, ~bind) body.

 The compile-time @rhombus(body, ~var) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use within a @rhombus(pattern) body.

}

@«macro.close_eval»(macro_eval)
