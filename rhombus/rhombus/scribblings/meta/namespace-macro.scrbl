#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "Namespace Space"){Namespace Macros}

@doc(
  space.transform namespace
){

 The @tech{space} for bindings of namespace names, such as the
 identifier immediate after the @rhombus(namespace) definition form.

}

@doc(
  defn.macro 'namespace.interleave:
                $option
                ...
                $body
                ...'

  grammar option:
    ~is_clause $expr
    ~is_clause: $body; ...
    ~parse_clause $expr
    ~parse_clause: $body; ...
    ~complete $expr
    ~complete: $body; ...
    ~init '$term ...; ...'
    ~init: '$term ...; ...'
    ~name $id_name
    ~name: $id_name
    ~defer_tail
    ~no_exports
){

 Intended as an expansion step for a macro that implements a new
 @rhombus(namespace)-like form, where procedures provided via
 @rhombus(~is_clause), @rhombus(~parse_clause), and @rhombus(~complete)
 interpose on steps of expansion so that form-specific clauses can be
 interleaved with definitions and expressions---similar to the way
 @rhombus(class) allows @tech(~doc: ref_doc){class clauses} to be
 interleaved with definitions and expressions in its body. The
 @rhombus(namespace.interleave) form does not necessarily define a
 namespace; a compile-time function provided by @rhombus(~complete)
 clause determines definitions, if any.

 Each @rhombus(option) keyword must appear at most once. The
 @rhombus(~is_clause), @rhombus(~parse_clause), @rhombus(~complete), and
 @rhombus(~init) @rhombus(option) forms are required.

@itemlist(

 @item{@rhombus(~is_clause): Followed by a compile-time @rhombus(expr)
  or @rhombus(body) sequence to produce a @rhombus(is_clause, ~var)
  function satisfying

@rhombusblock(#,(@rhombus((Group, Map.of(Symbol, Any)) -> Any.to_boolean, ~annot)))

  The @rhombus(Group, ~annot) argument is a form among the overall
  @rhombus(body) forms (or a partially expanded form of some
  @rhombus(body)). The @rhombus(Map, ~annot) argument serves as additional
  arguments to @rhombus(is_clause, ~var), where the set of keys in the map
  can be extended in the future; the map is always empty.

  The @rhombus(is_clause) function should return a true if value if the
  form should be passed to the @rhombus(~parse_clause) function for
  expansion, @rhombus(#false) otherwise.}

 @item{@rhombus(~parse_clause): Followed by a compile-time
  @rhombus(expr) or @rhombus(body) sequence to produce a
  @rhombus(parse_clause, ~var) function satisfying

@rhombusblock(#,(@rhombus((Group, Syntax, Map.of(Symbol, Any)) -> values(Syntax, Syntax), ~annot)))

  The @rhombus(Group, ~annot) argument is one for which the
  @rhombus(is_parse, ~var) function just produced a true value. The
  @rhombus(Syntax, ~annot) argument is the current state of expansion as
  initialized by the @rhombus(~init) form. The @rhombus(Map, ~annot)
  argument serves as additional arguments to @rhombus(is_clause, ~var),
  where the set of keys in the map can be extended in the future.

  The @rhombus(parse_clause, ~var) function must return two values: a
  multi-group syntax object to splice in place of the clause to continue
  expansion, and a syntax object representing the new state of the
  expansion to be passed back to future calls to
  @rhombus(parse_clause, ~var) or to the @rhombus(~complete) function.

  The @rhombus(Map, ~annot) argument maps two keys (but may be extended
  with new keys in the future):

@itemlist(

  @item{@rhombus(#'close_expr): A function that accepts a
   @rhombus(Group, ~annot) for an expression and produces an opaque
   expression that preserves @tech{syntax parameters} in effect for the
   current @rhombus(body) form.}

  @item{@rhombus(#'close_defn): A function that accepts a
   @rhombus(Syntax, ~annot) for a definition sequence and produces an
   opaque definition form that preserves @tech{syntax parameters} in effect
   for the current @rhombus(body) form.}

)}

 @item{@rhombus(~complete): Followed by a compile-time
  @rhombus(expr) or @rhombus(body) sequence to produce a
  @rhombus(complete, ~var) function satisfying

@rhombusblock(#,(@rhombus((Syntax, Map.of(Symbol, Any)) -> Syntax, ~annot)))

  The @rhombus(Syntax, ~annot) argument is the current state of expansion
  as initialized by the @rhombus(~init) form and updated by calls to
  @rhombus(parse_clause, ~var). The @rhombus(Map, ~annot) argument serves
  as additional arguments to @rhombus(complete, ~var), where the set
  of keys in the map can be extended in the future.

  The @rhombus(complete, ~var) function must return a multi-group syntax
  object to splice after the @rhombus(namespace.interleave) form in its
  enclosing context.

  The @rhombus(Map, ~annot) argument potentially maps the following keys
  (but may be extended with new keys in the future):

@itemlist(

  @item{@rhombus(#'exports): An @rhombus(namespace_meta.Exports) object
   that encapsulates @rhombus(export) declarations among the expanded
   @rhombus(body) forms. The object includes a
   @rhombus(namespace_meta.Exports.declaration) method for generating a
   namespace declaration with the recorded exports. The @rhombus(#'exports)
   key is not present if @rhombus(~no_exports) is present as a
   @rhombus(option).}

  @item{@rhombus(#'tail): A multi-group syntax object holding
   @rhombus(body) forms that follow the last @rhombus(body) form that is a
   @tech{nestable declaration} or a clause a recognized by
   @rhombus(is_clause, ~var). This key is present only when
   @rhombus(~defer_tail) is present as a @rhombus(option).}

)

}

 @item{@rhombus(~init): Followed by a quoted literal syntax object that
  represents the state of expansion. This value is updated by each
  @rhombus(parse_clause, ~var) call and delivered in the end to
  @rhombus(complete, ~var).}

 @item{@rhombus(~defer_tail): Causes @rhombus(body) expansion to stop at
  a point where only definitions and expressions remain. See
  @rhombus(~complete) for more information.}

 @item{@rhombus(~no_exports): Disallows the use of @rhombus(export) (or,
  more generally, a @tech{nestable declaration}) as a @rhombus(body) form.}

)

}


@doc(
  ~meta
  class namespace_meta.Exports():
    constructor ~none
){

 Encapsulates @rhombus(export, ~decl)s in the body of a
 @rhombus(namespace.interleave) form. See @rhombus(~complete) in
 @rhombus(namespace.interleave) for more information.

}

@doc(
  ~meta
  method (ex :: namespace_meta.Exports).declaration(
    name :: Identifier
  ) :: Group
){

 Returns an opaque @rhombus(namespace name) definition with exports
 encapsulated by @rhombus(ex). Note that @rhombus(namespace_meta.Exports.add)
 can add to the set of exports.

}

@doc(
  ~meta
  method (ex :: namespace_meta.Exports).external_names()
    :: List.of(Identifier)
  method (ex :: namespace_meta.Exports).include_spaces()
    :: List.of(Any.of(#'all) || List.of(Symbol))
  method (ex :: namespace_meta.Exports).exclude_spaces()
    :: List.of(List.of(Symbol))
){

 Reports information about the exports that are encapsulated by
 @rhombus(ex). The results for each method are parallel to results from
 the other methods; for example, the first element of the result of
 @rhombus(namespace_meta.Exports.include_spaces) applies to the first
 identifier in the result of
 @rhombus(namespace_meta.Exports.external_names).

 Each external name from @rhombus(namespace_meta.Exports.external_names)
 corresponds to a distinct symbol.

 Each result from @rhombus(namespace_meta.Exports.include_spaces)
 describes which spaces are included by the export, where @rhombus(#'all)
 means all spaces except as excluded in the corresponding list from
 @rhombus(namespace_meta.Exports.exclude_spaces).

 Each result from @rhombus(namespace_meta.Exports.exclude_spaces)
 describes which spaces are not included by the export when the
 corresponding result from
 @rhombus(namespace_meta.Exports.include_spaces) is @rhombus(#'all).

}

@doc(
  ~meta
  method (ex :: namespace_meta.Exports).add(
    external_name :: Identifier,
    internal_name :: Identifier,
    include_spaces :: Any.of(#'all) || List.of(False || Symbol),
    exclude_spaces :: List.of(Symbol)
  ) :: namespace_meta.Exports
){

 Returns an updated @rhombus(namespace_meta.Exports, ~class) that
 extends the encapsulated exports with the supplied identifier.

}
