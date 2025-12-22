#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@(def dollar = @rhombus($))

@title{Unquote Binding Macros}

@deftech{Unquote binding} forms are similar to normal binding forms, but
they appear only under @rhombus($, ~bind) within a syntax binding
pattern. Unquote binding forms are distinct from normal binding forms
because they must match syntax objects; some operators work in both
contexts but have different meanings, such as
@rhombus(::, ~unquote_bind) and @rhombus(_, ~unquote_bind) for unquote
bindings versus @rhombus(::, ~bind) and
@rhombus(_, ~bind) for normal bindings.

@doc(
  space.enforest unquote_bind
){

 The @tech{space} for bindings of identifiers and operator that implement
 @rhombus($, ~bind) escape patterns.

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn

  defn.macro 'unquote_bind.macro $macro_patterns'

  grammar more_options
  | ~kind $id
  | ~kind: $id

){

 Like @rhombus(expr.macro), but for binding an identifier or operator
 that works within a @rhombus($, ~bind) escape for a syntax pattern.
 The macro is bound in the @rhombus(unquote_bind, ~space) @tech{space}.

 In addition to the @rhombus(option) forms supported by
 @rhombus(expr.macro), @rhombus(unquote_bind.macro) supports a
 @rhombus(~kind) option, which declares an identifier be bound to
 context-kind information:

@itemlist(

  @item{@rhombus(#'term): A context that needs a matcher for terms or spliced sequences.}

  @item{@rhombus(#'grouplet): A context where the
  @rhombus(Group, ~stxclass) syntax class could be used to match a
  nonempty term sequence.}

  @item{@rhombus(#'group): A context to match a whole group, which is
  like @rhombus(#'grouplet), but where an identifier defaults to the
  @rhombus(Group, ~stxclass) syntax class instead of
  @rhombus(Term, ~stxclass).}

  @item{@rhombus(#'multi): A context to match a sequence of groups.}

  @item{@rhombus(#'block): A context to match a sequence of groups
  within a block.}

)

 When it encounters a @rhombus($, ~bind) escape, the
 @rhombus(#%quotes, ~bind) binding form tries these contexts last to
 first for possibilities that apply at the use site. If an unquote
 binding form is not compatible with the given context, it can return
 @rhombus(unquote_bind_meta.pack_invalid) to have the next possibility
 tried. A new form with unquote bindings positions should similarly try
 applicable contexts in order. Compound unquote binding forms like
 @rhombus(&&, ~unquote_bind) and @rhombus(||, ~unquote_bind), meanwhile,
 propagate failure for subforms, which means that a new unquote binding
 form that expands to existing forms can usually rely on context
 fallbacks to work automatically.

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
    unquote_bind.macro 'wrapped $(id :: Identifier)':
      '_ :: Wrapped: content as $id'
    match '{x} [y] (z)'
    | '$(wrapped a) ...': [a, ...]
)

}


@doc(
  ~meta
  fun unquote_bind_meta.unpack_kind(stx :: Term)
    :: Any.of(#false, #'term, #'grouplet, #'group, #'multi, #'block, #'id)
  fun unquote_bind_meta.pack_invalid() :: Term
){

 The @rhombus(annot_meta.unpack_kind) function takes a parsed unquote
 binding form and reports the kind of context that it was parsed for. A
 @rhombus(#false) result indicates that parsing in a given context was
 unsuccessful. A @rhombus(#'id) result indicates that @rhombus(stx) can
 adapt to any context, like an identifier can.

 The @rhombus(unquote_bind_meta.pack_invalid) function returns a syntax
 object that represents a failure of an unquote binding form for a
 particular context. This kind of failure normally triggers a retry in a
 narrower kind of context.

}


@doc(
  ~meta
  syntax_class unquote_bind_meta.Parsed(
    kind :: Any.of(#'term, #'grouplet, #'group, #'multi, #'block)
  ):
    kind: ~group
    fields:
      group
  syntax_class unquote_bind_meta.AfterPrefixParsed(
    op_name :: Name,
    kind :: Any.of(#'term, #'grouplet, #'group, #'multi, #'block)
  ):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class unquote_bind_meta.AfterInfixParsed(
    op_name :: Name,
    kind :: Any.of(#'term, #'grouplet, #'group, #'multi, #'block)
  ):
    kind: ~group
    fields:
      group
      [tail, ...]
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for
 unquote bindings.

 Unlike @rhombus(expr_meta.Parsed, ~stxclass), a @rhombus(kind) argument
 specifies the kind of context for the escape as described in
 @rhombus(unquote_bind.macro).

}


@(macro.close_eval(macro_eval))
