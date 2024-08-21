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

 Combines a set of functions into a value suitabel for use with
 @rhombus(doc.bridge):

@itemlist(

 @item{@rhombus(extract_desc): Takes a syntax object and returns a
  string or list of strings describing the documented binding or bindings.
  If a list of strings is returned, all the other functions should produce
  a list with the same number of values for the given syntax object. A
  description string is shown in the definition box, such as
  @rhombus("function") for function documentation.}

 @item{@rhombus(extract_space): Takes a syntax object and returns a
  space or lists of spaces. Each space is a keyword for a built-in space
  (see @rhombus(rhombus)), a symbol, or @rhombus(#false). As a
  convenience, the first returned space is passed back to the reminding
  functions.}

 @item{@rhombus(extract_defined): Takes a syntax object and a space
  (produced by @rhombus(extract_space) and returns a defined name or list of
  defined names. A defined name is either

  @itemlist(

   @item{an identifier that is defined;},

   @item{a list containing two identifiers, where the first identifier
   corresponds to a namespace and the second is the defined name within
   that namespace; or}

   @item{a list containing two identifiers and a string, which is like
   the previous two cases, but the string is typeset literally instead of
   having the typeset form derived from the second identifier.}

  )}

 @item{@rhombus(extract_metavariables): Takes a syntax object, a space
  (produced by @rhombus(extract_space), and a set of names to be typeset
  as metavariables. The result is a new set of metavariables. Add to the
  set using @rhombus(doc_meta.add_metavariable).}

 @item{@rhombus(extract_metavariables): Takes a syntax object, a space
  (produced by @rhombus(extract_space), and a function that converts a
  syntax object at the location of the defined name and produces a syntax
  object for the typset replacement. The result should be a syntax object
  to use as an expression that produces the typeset form, typically
  produced via @rhombus(doc_meta.typeset_rhombusblock).}

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
