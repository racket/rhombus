#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      scribble/spacer open
      scribble/doc_meta open)

@title(~tag: "spacer"){Code Binding-Space Adjusters}

@docmodule(~use_sources: lib("scribble/spacer.rhm"),
           scribble/spacer)

A @deftech{spacer} is a compile-time function that is used by forms like
@rhombus(rhombus) to determine how names within a typeset form should be
linked to documentation. For example, @rhombus(::, ~datum) in some
contexts should be linked to the @rhombus(::) expression operator, and
in other contexts to the @rhombus(::, ~bind) binding operator. In
general, typeset code cannot be executed or expanded to determine those
bindings. Instead, links are determined by a combination of
@rhombus(meta_label, ~expo) imports and spacer-applied properties.
Spacers thus provide an approximate and best-effort attempt to properly
link names in documentation.

A spacer can also communicate indirect binding information to support
linking of names after @rhombus(.). For example, the
@rhombus(length, ~datum) method name in @rhombus([1, 2, 3].length())
should be linked to the documentation of @rhombus(List.length),
independent of whatever @rhombus(meta_label, ~expo) binding the
identifier @rhombus(length, ~datum) length might have. See
@secref("spacer-props") for more information.

@doc(
  ~nonterminal:
    self_id: block id
    tail_id: block id
    context_id: block id
    esc_id: block id
    left_id: block id
  defn.macro 'bridge $name($maybe_left, $self_id, $tail_id, $context_id, $esc_id):
                ~in: $space ...
                $body
                ...'

  grammar maybe_left:
    ~left $left_id
    #,(epsilon)
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

 If a @rhombus(~left) argument is declared, then the spacer can be
 called in either infix or prefix mode. When called in prefix mode, the
 argument for @rhombus(~left) will be an empty syntax object,
 @rhombus('').

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
  fun adjust_rest_sequence(head :: TermSequence,
                           stx :: TermSequence, context :: Context,
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

 The @rhombus(adjust_rest_sequence) function supports the possibility of
 an infix spacer at the start of @rhombus(stx), in which case
 @rhombus(head) is propagated to the spacer as the @rhombus(~left)
 argument.

}


@section(~tag: "spacer-props"){Spacers and Indirect Binding}

In addition to setting the space of a name, a @tech{spacer} can add
syntax properties that communicate binding and annotation information.
Resolving these properties some requires extra context as provided by
the documentation of a form, such as the annotation for a function's
result.

For example, to implement the connection that links the
@rhombus(length, ~datum) method name in @rhombus([1, 2, 3].length()) to
@rhombus(List.length), the spacer for @rhombus(#%brackets, ~datum) adds
a @rhombus(#'annot) property to the @rhombus([1, 2, 3]) term with the
value @rhombus('List', ~datum); then, the method @rhombus(List.length) can
be inferred by treating the annotation name as a namespace.

In the case of @rhombus(to_string(1).length()), the term before
@rhombus(.) is @rhombus((1)), but that term is part of a function call.
The @rhombus(#%call, ~datum) infix spacer adds a property to
@rhombus((1)) to say that its annotation is effectively the result of
calling @rhombus(to_string). The result annotation for
@rhombus(to_string) is determined to be @rhombus(String, ~annot) via
cross-reference information stored by the @rhombus(fun, ~doc)
documentation form as used to document @rhombus(to_string).

The following syntax property keys (see @rhombus(Syntax/property)) are
recognized; take care to add them as preserved properties:

@itemlist(

 @item{@rhombus(#'spacer_key): used by other syntax objects to refer to
 the one with the property. References to spaced terms generally make
 sense only with in a set of syntax objects that are compiled an
 serialized together, so the value of @rhombus(#'spacer_key) can be a
 gensym created by @rhombus(Symbol.gen).}

 @item{@rhombus(#'field): on a field identifier to connect it to an
 annotation-as-namespace that exports the field, typically by referring
 to another term. A @rhombus(#'field) value can have one of several
 recognized shapes:

 @itemlist(

   @item{@rhombus(Pair(#'of, #,(@rhombus(key_symbol, ~var)))): refers to
  another term by it's @rhombus(#'spacer_key) value, where that term
  should have an @rhombus(#'annot) property to describe an annotation name
  that is used as namespace name.}

  )}

 @item{@rhombus(#'annot): on a field identifier to connect it to a
 namespace name that exports the field, typically by referring to another
 term. A @rhombus(#'field) value can have one of several recognized
 shapes:

 @itemlist(

   @item{@rhombus(Pair(#'as, #,(@rhombus(key_symbol, ~var)))): refers to
  another term by it's @rhombus(#'spacer_key) value, where that term
  should be an identifier that is used as the namespace name.}

   @item{@rhombus(Pair(#'as_export, id)): provides a namespace name
  directly as @rhombus(id).}

   @item{@rhombus(Pair(#'result, #,(@rhombus(key_symbol, ~var)))): refers to
  another term by it's @rhombus(#'spacer_key) value, where that term
  should be an identifier that is hperlinked as a function, and the
  function's result annotation (as recorded in documentation) is used as
  the namespace name.}

)

 Some inference steps may require cross-reference information from
 documentation, such as the result annotation of a function. That
 cross-reference information can be provided by a documentation form that
 is bound by @rhombus(doc.bridge), implemented with
 @rhombus(doc_meta.transformer), and through an
 @rhombus(~extract_spacer_infos) function that produces a map with the
 following recognized keys:

@itemlist(

 @item{@rhombus(#'result_annotation): an identifier for the result of a
  documented function, method, or property. The identifier is used as a
  namespace for the purpose of finding methods and properties within that
  result.}

)

}

)
