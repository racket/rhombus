#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Unquote Binding Macros}

@deftech{Unquote binding} forms are similar to normal binding forms, but
they appear only under @rhombus($, ~bind) within a syntax binding
pattern. Unquote binding forms are distinct from normal binding forms
because they must match syntax objects; some operators wotk in both
contexts but have different meanings, such as
@rhombus(::, ~unquote_bind) and @rhombus(#', ~unquote_bind) for unquote
bindings versus @rhombus(::, ~bind) and
@rhombus(#', ~bind) for normal bindings.

@doc(
  space.enforest unquote_bind
){

 The @tech{space} for bindings of identifiers and operator that implement
 @rhombus($, ~bind) escape patterns.

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro
  defn.macro 'unquote_bind.macro $macro_patterns'
){

 Like @rhombus(expr.macro, ~expr), but for binding an identifier or operator
 that works within a @rhombus($, ~bind) escape for a syntax pattern.
 The macro is bound in the @rhombus(unquote_bind, ~space) @tech{space}.

@examples(
  ~eval: macro_eval
  ~repl:
    unquote_bind.macro 'dots':
      '«'$('...')'»'
    match Syntax.make_group(['...', '...', '...'])
    | '$dots ...': "all dots"
  ~repl:
    syntax_class Wrapped
    | '($content)'
    | '[$content]'
    | '{$content}'
    unquote_bind.macro 'wrapped $(id :: Id)':
      '_ :: Wrapped: content as $id'
    match '{x} [y] (z)'
    | '$(wrapped a) ...': [a, ...]
)

}

@«macro.close_eval»(macro_eval)
