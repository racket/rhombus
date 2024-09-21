#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      scribble/doc_meta open
      scribble/spacer open)

@title(~tag: "doc_meta"){Creating Documentation Forms}

@docmodule(~use_sources: lib("scribble/doc_meta.rhm"),
           scribble/doc_meta)

@doc(
  space.transform doc
  def doc_meta.space :: SpaceMeta
){

 A space for documentation forms.

}

@doc(
  defn.macro 'doc.bridge $name:
                $body
                ...'
){

 Binds @rhombus(name) as a documentation form in the
 @rhombus(doc, ~space) space for use with the @rhombus(doc) documentation
 form. The @rhombus(body)s should produce a value created by
 @rhombus(doc_meta.transformer).

}

@doc(
  fun doc_meta.transformer(
    ~extract_desc: extract_desc :: Function.of_arity(1),
    ~extract_space: extract_space :: Function.of_arity(1),
    ~extract_defined: extract_defined :: Function.of_arity(2),
    ~extract_metavariables: extract_metavariables :: Function.of_arity(3),
    ~extract_typeset: extract_typeset :: Function.of_arity(3)
  )
){

 Combines a set of functions into a value suitable for use with
 @rhombus(doc.bridge):

@itemlist(

 @item{@rhombus(extract_desc): Takes a syntax object and returns a
  string or list of strings describing the documented binding or bindings.
  A description string is shown in the definition box, such as
  @rhombus("function") for function documentation.

  If a list of strings is returned, the @rhombus(extract_space) and
  @rhombus(extract_defined) functions should produce a list with the same
  number of values for the given syntax object. The
  @rhombus(extract_metavariables) will receive a list of space lists, and
  it will also receive a list of converter functions.}

 @item{@rhombus(extract_space): Takes a syntax object and returns a
  space, a list of spaces, or a lists of space lists; the result must be a
  list of space lists if @rhombus(extract_desc) returned a list. Each
  space is a keyword for a built-in space (see @rhombus(rhombus)), a
  symbol, or @rhombus(#false). A space list indicates a list of spaces
  where the identifier is bound (e.g., the @rhombus(class) binds in the
  @rhombus(~class) and @rhombus(~annot) spaces).

  As a convenience, the result is is passed back to the
  @rhombus(extract_defined) and @rhombus(extract_metavariables) functions,
  except that only the first list of spaces is passed back when
  @rhombus(extract_space) returns a list of lists.}

 @item{@rhombus(extract_defined): Takes a syntax object and a space or
  space list (produced by @rhombus(extract_space)) and returns a defined
  name or list of defined names. A defined name is either

  @itemlist(

   @item{an identifier that is defined (which is equivalent to a map
   from @rhombus(#'target) to the identifier);},

   @item{a map where the following keys are recognized:

   @itemlist(

    @item{@rhombus(#'target) (required): An identifier that is defined.
    More precisely, this identifier's binding in the label meta phase is the
    defined binding.}

    @item{@rhombus(#'root): An identifier that corresponds to a
    namespace, where the @rhombus(#'target) is defined within the namespace.
    If this key is not present or it is mapped to @rhombus(#false), then no
    namespace prefix is shown.}

    @item{@rhombus(#'raw): A string to use for showing the defined name.
    If this key is not present, the raw text of the @rhombus(#'target)
    identifier is used.}

    @item{@rhombus(#'raw_prefix): A string that should prefix the
    defined name, but it is not part of the defined name for searching, and
    the prefix is rendered in a way to indicate that it's providing context
    instead of naming a module export. This prefix is intended to be a
    module prefix for exports that are meant to be used in dotted form,
    instead of through an @rhombus(open, ~impo)ed module import.}

   )}

  )}

 @item{@rhombus(extract_metavariables): Takes a syntax object, a space
  or space list (produced by @rhombus(extract_space)), and a set of names
  to be typeset as metavariables. The result must be a new set of
  metavariables. Add to the set using
  @rhombus(doc_meta.add_metavariable).}

 @item{@rhombus(extract_typeset): Takes a syntax object, a space, space
  list, or list of space lists (produced by @rhombus(extract_space)), and
  a function or list of converter functions. Each converter function
  converts a syntax object at the location of the defined name and
  produces a syntax object for the typeset replacement. The result should
  be a syntax object to use as an expression that produces the typeset
  form, typically produced via @rhombus(doc_meta.typeset_rhombusblock).

  Each convert function accepts optional arguments:

  @itemlist(

   @item{@rhombus(~as_wrap): a true value (the default) indicates that
   the result should include an escape, so it's suitable for splicing
   into a @rhombus(rhombusblock) form.}

   @item{@rhombus(~as_redef): a true value (@emph{not} the default)
   indicates that the result should should not be a hyperlink target,
   because it's a secondary definition of the binding.}

   @item{@rhombus(~as_meta): a true value (@emph{not} the default)
   indicates that the result should be typeset as a metavariable (usually
   italic) instead of a variable.}

  )}

)

}

@doc(
  fun head_extract_name(
    stx :: Syntax, space :: SpaceName
  ) :: Identifier || List
  fun parens_extract_name(
    stx :: Syntax, space :: SpaceName
  ) :: Identifier || List
  fun operator_macro_extract_name(
    stx :: Syntax, space :: SpaceName
  ) :: Identifier || List
  fun identifier_macro_extract_name(
    stx :: Syntax, space :: SpaceName
  ) :: Identifier || List

  fun head_extract_metavariables(
    stx :: Syntax, space :: SpaceName, vars :: Map
  ) :: Map
  fun parens_extract_metavariables(
    stx :: Syntax, space :: SpaceName, vars :: Map
  ) :: Map
  fun operator_macro_extract_metavariables(
    stx :: Syntax, space :: SpaceName, vars :: Map
  ) :: Map
  fun identifier_macro_extract_metavariables(
    stx :: Syntax, space :: SpaceName, vars :: Map
  ) :: Map

  fun head_extract_typeset(
    stx :: Syntax, space :: SpaceName, subst
  ) :: Syntax
  fun parens_extract_typeset(
    stx :: Syntax, space :: SpaceName, subst
  ) :: Syntax
  fun operator_macro_extract_typeset(
    stx :: Syntax, space :: SpaceName, subst
  ) :: Syntax
  fun identifier_macro_extract_typeset(
    stx :: Syntax, space :: SpaceName, subst
  ) :: Syntax
){

 Extraction functions for use with @rhombus(doc_meta.transformer) that
 handle common documentation patterns.

}

@doc(
  fun doc_meta.add_metavariable(
    vars :: Map,
    id :: Identifier,
    ~is_nonterminal = #false
  ) :: Map
){

 For use by the @rhombus(~extract_metavariables) component of a
 @rhombus(doc_meta.transformer) construction.

}

@doc(
  fun doc_meta.typeset_rhombusblock(
    form :: Syntax,
    ~at: at_form :: Syntax = form,
    ~is_pattern = #false,
    ~options: options :: Term = '(~inset: #false)'
  ) :: Syntax
){

 For use by the @rhombus(~extract_typeset) component of a
 @rhombus(doc_meta.transformer) construction.

}
