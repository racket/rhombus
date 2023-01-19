#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{For Clause Macros}

@doc(
  space.transform for_clause
){

 The @tech{space} for bindings of identifiers that implement
 @rhombus(for) clauses.

}

@doc(
  defn.macro '«for_clause.macro '$defined_name $pattern ...':
                 $option; ...
                 $body
                 ...»'
  defn.macro '«for_clause.macro
               | '$defined_name $pattern ...':
                   $option; ...
                   $body
                   ...
               | ...»'
){

 Similar to @rhombus(defn.macro), but defines a name as a clause form
 for use within a @rhombus(for) body.

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

@doc(
  defn.macro 'for_clause.only.macro $macro_decl'
){

 Like @rhombus(for_clause.macro), but the identifier or operator is bound
 only in the @rhombus(for_clause, ~space) @tech{space}.

}

@«macro.close_eval»(macro_eval)
