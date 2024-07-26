#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

@title{Syntax Class Clause Macros}

@doc(
  space.transform syntax_class_clause
){

 The @tech{space} for bindings of identifiers that implement
 @rhombus(syntax_class) clauses.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
    body: block

  defn.macro 'syntax_class_clause.macro $prefix_macro_patterns'
){

 Similar to @rhombus(defn.macro), but defines a name in the
 @rhombus(syntax_class_clause, ~space) @tech{space} as a clause form
 for use within a @rhombus(syntax_class) body.

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a block of groups optionally followed by syntax patterns
 to be spliced in place of the macro use within a @rhombus(syntax_class) body.
 The spliced syntax patterns can be supplied at most once.

@examples(
  ~eval: macro_eval
  ~defn:
    syntax_class_clause.macro 'maybe_block $id $rhs_id':
      '«: fields: [$rhs_id, $('...')]
        | '$id: $('$')$rhs_id; $('...')'
        | '$id $('$')rhs0 $('...')':
            field [$rhs_id, $('...')] = ['$('$')rhs0 $('...')']»'
  ~defn:
    syntax_class Options:
      maybe_block options rhs
  ~repl:
    match 'options 1 2'
    | '$(o :: Options)': [o.rhs, ...]
    match 'options:
             1 2
             3 4'
    | '$(o :: Options)': [o.rhs, ...]
)

}


@(macro.close_eval(macro_eval))
