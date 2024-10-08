#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      scribble/spacer open)

@title(~tag: "spacer"){Code Binding-Space Adjusters}

@docmodule(~use_sources: lib("scribble/spacer.rhm"),
           scribble/spacer)

A @defterm{spacer} is a compile-time function that is used by forms like
@rhombus(rhombus) to determine how names within a typeset form should be
linked to documentation. For example, @rhombus(::, ~datum) in some
contexts should be linked to the @rhombus(::) expression operator, and
in other contexts to the @rhombus(::, ~bind) binding operator. In
general, typeset code cannot be executed or expanded to determine those
bindings. Instead, links are determined by a combination of
@rhombus(meta_label, ~expo) imports and spacer-applied properties.
Spacers thus provide an approximate and best-effort attempt to properly
link names in documentation.

@doc(
  ~nonterminal:
    self_id: block id
    tail_id: block id
    context_id: block id
    esc_id: block id
  defn.macro 'bridge $name($self_id, $tail_id, $context_id, $esc_id):
                ~in: $space ...
                $body
                ...'
){

 Binds @rhombus(name) in the typesetting space to a function that
 specifies space choices for terms within a @rhombus(name) form. For
 example, @rhombus(fun) is bound to a function that (along other things)
 looks for a result annotation in a @rhombus(fun) form and specifies that
 the annotation is in the @rhombus(#'~annot) space. The spacing function
 will be used only when @rhombus(name) itself is in one of the spaces
 named by a @rhombus(space), where each @rhombus(space) is either a
 keyword for a built-in space (such as @rhombus(#'~annot); see
 @rhombus(rhombus)) or a symbol naming a space.

 The @rhombus(body) result should be a syntax object like
 @rhombus('$self_id $tail_id'), but where space properties are added
 within @rhombus(self_id) and @rhombus(tail_id) via functions like
 @rhombus(set) and @rhombus(adjust_term). Take care to preserve syntax
 locations and properties via @rhombus(Syntax.relocate) when
 reconstructing syntax objects; functions like @rhombus(adjust_term) use
 @rhombus(Syntax.relocate) automatically, but can obviously only cover
 the given syntax object.

 The @rhombus(context_id) argument will be a keyword, symbol, or list of
 symbols corresponding to one of the @rhombus(space)s. The spacer's
 behavior might depend on that context, but often it's passed along as-is
 to functions like @rhombus(set) or @rhombus(adjust_group).

 The @rhombus(esc_id) argument is an identifier or operator syntax object
 that specifies an escape operator, such as @rhombus(#,). Spacing should
 general not traverse into escaped forms.

}

@doc(
  annot.macro 'SpaceName'
  annot.macro 'Context'
){

 A @rhombus(SpaceName, ~annot) represents a space as a symbol, keyword
 for a built-in space (see @rhombus(rhombus)), or @rhombus(#false) for
 the @rhombus(expr, ~space) space. A @rhombus(Context, ~annot) is a
 @rhombus(SpaceName, ~annot), list of @rhombus(SpaceName, ~annot)s, or
 pair list of @rhombus(SpaceName, ~annot)s.

 A @rhombus(Context, ~annot) represents one or more spaces for recording
 as a property or adding further space annotations. When a
 @rhombus(Context, ~annot) is a list to be recorded in a property, space
 symbols in the list will be tried first to last to look for link-target
 bindings within each space.

}

@doc(
  fun set(stx :: TermSequence, context :: Context) :: Sequence
  fun set_group(stx :: Group, context :: Context) :: Group
){

 Returns a syntax object like @rhombus(stx), but with properties to
 specify that it should be rendered as being in the space indicated by
 @rhombus(context).

}

@doc(
  fun adjust_term(stx :: Term, context :: Context, esc :: Name)
    :: Syntax
  fun adjust_group(stx :: Group, context :: Context, esc :: Name)
    :: Syntax
  fun adjust_sequence(stx :: TermSequence, context :: Context,
                      esc :: Name)
    :: Syntax
  fun adjust_multi(stx :: Syntax, context :: Context, esc :: Name)
    :: Syntax
  fun adjust_block(stx :: Block, context :: Context, esc :: Name)
    :: Syntax
){

 Similar to @rhombus(set), but recursively finds and applies spacers
 within @rhombus(stx) according to the given @rhombus(context), passing
 along @rhombus(esc) to nested spacers so that they can avoid traversing
 escape sequences.

}
