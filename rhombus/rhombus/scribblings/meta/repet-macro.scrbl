#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@title{Repetition Macros}

@doc(
  space.transform repet
){

 The @tech{space} for bindings of @tech(~doc: ref_doc){repetitions}.

}


@doc(
  ~meta
  def repet_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(repet, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn

  defn.macro 'repet.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines an identifier or operator as a
 repetition form in the @rhombus(repet, ~space) @tech{space}.
 The result of the macro expansion can be a low-level
 binding description created with @rhombus(repet_meta.pack_list).

@examples(
  ~eval: macro_eval
  ~defn:
    repet.macro 'nat3':
      ~op_stx self
      repet_meta.pack_list('($self, [0, 1, 2], 1, 0, ())')
  ~repl:
    [nat3, ...]
)

 See @secref("stxobj-track") for information about expansion tracking,
 which applies automatically for some pattern forms in
 @rhombus(macro_patterns).

}


@doc(
  ~meta
  fun repet_meta.pack_generator(stx :: Syntax) :: Syntax
){

 Packs the implementation of a repetition form to serve as the result of
 expanding a repetition macro, where the repetition is described in terms
 of @rhombus(for) clauses and a body.

 The syntax object @rhombus(stx) must have the following shape:

 @rhombusblock(
  '(
     #,(@rhombus(source_form, ~var)),
     ((((#,(@rhombus(id, ~var)), ...), #,(@rhombus(seq_expr, ~var))),
       ...),
      ...),
     #,(@rhombus(body_expr, ~var)),
     #,(@rhombus(use_depth, ~var)),
     ((#,(@rhombus(static_key, ~var)), #,(@rhombus(static_value, ~var))), ...)
   )'
 )

 The @rhombus(source_form, ~var) group is used for error reporting to
 show the repetition, such as when the repetition is used at the wrong
 depth.

 Each @rhombus((#,(@rhombus(id, ~var)), ...)) sequence names values of
 elements drawn from the corresponding @rhombus(seq_expr, ~var). Each
 inner sequence of @rhombus(seq_expr, ~var)s corresponds to sequences
 traversed in parallel, while the outermost sequence corresponds to sets
 of sequences that are nested. The length of the outermost sequence
 determines the depth of the repetition.

 The @rhombus(body_expr, ~var) expression produces elements of the
 repetition, usually referring to the @rhombus(id, ~var)s that are bound
 to input sequence elements, and only the @rhombus(id, ~var)s that are in
 the last parallel group.

 The @rhombus(use_depth, ~var) integer specifies how much additional
 depth has already been extracted from an original repetition; a non-zero
 @rhombus(use_depth, ~var) might be relevant to reporting the use of a
 repetition at the wrong depth in terms of the original form's depth.

 The @rhombus(static_key, ~var)--@rhombus(static_value, ~var) pairs
 describe ``upward'' static information for individual elements of the
 repetition. This information is automatically packed via
 @rhombus(statinfo_meta.pack).

}


@doc(
  ~meta
  fun repet_meta.unpack_generator(stx :: Syntax) :: Syntax
){

 The inverse of @rhombus(repet_meta.pack_generator), which is useful for
 unpacking information about the expansion of nested repetition forms.

 Any repetition can be unpacked both this way and by using
 @rhombus(repet_meta.unpack_list), but the output
 @rhombus(repet_meta.unpack_generator) is more flexible for most
 purposes.

@examples(
  ~eval: macro_eval
  ~defn:
    repet.macro 'enumed($from, $(sub :: repet_meta.Parsed))':
      ~all_stx stx
      let '($_, ($binds, ..., ($inner_bind, ...)), $body, $use_depth, $_)':
        repet_meta.unpack_generator(sub)
      let (_, si):
        let '$(p :: annot_meta.Parsed)' = 'List'
        annot_meta.unpack_predicate(p)
      repet_meta.pack_generator(
        '($stx,
          ($binds, ..., ($inner_bind, ..., ((i), $from ..))),
          [i, $body],
          $use_depth,
          $si)'
      )
  ~repl:
    def [x, ...] = ["a", "b", "c"]
    [enumed(1, x), ...]
)



}


@doc(
  ~meta
  fun repet_meta.pack_list(stx :: Syntax) :: Syntax
){

 Packs the implementation of a repetition form to serve as the result of
 expanding a repetition macro, where elements of the repetition are
 provided through a list. See @rhombus(repet_meta.pack_generator) for
 an alternative representation.

 The syntax object @rhombus(stx) must have the following shape:

 @rhombusblock(
  '(
     #,(@rhombus(source_form, ~var)),
     #,(@rhombus(list_expr, ~var)),
     #,(@rhombus(depth, ~var)),
     #,(@rhombus(use_depth, ~var)),
     ((#,(@rhombus(static_key, ~var)), #,(@rhombus(static_value, ~var))), ...)
   )'
 )

 The @rhombus(source_form, ~var) group is used for error reporting to
 show the repetition, such as when the repetition is used at the wrong
 depth.

 The @rhombus(name_id, ~var) term is for error reporting and reflection
 in the sense that it is used as the inferred name for an element of the
 repetition, in case such a name is relevant.

 The @rhombus(list_expr, ~var) expression produces a @rhombus(List, ~annot) that contains
 the elements of the repetition. Lists must be nested according to the
 repetition's depth: a list of elements for depth 1, a list of element
 lists for depth 2, and so on.

 The @rhombus(depth, ~var) integer specifies the depth of the
 repetition. The @rhombus(use_depth, ~var) specifies how much additional
 depth has already been extracted from an original repetition; a non-zero
 @rhombus(use_depth, ~var) might be relevant to reporting the use of a
 repetition at the wrong depth in terms of the original form's depth.

 The @rhombus(static_key, ~var)--@rhombus(static_value, ~var) pairs
 describe ``upward'' static information for individual elements of the
 repetition. This information is automatically packed via
 @rhombus(statinfo_meta.pack).

}

@doc(
  ~meta
  fun repet_meta.unpack_list(stx :: Syntax) :: Syntax
){

 The inverse of @rhombus(repet_meta.pack_list), which is useful for
 unpacking information about the expansion of nested repetition forms.

 Any repetition can be unpacked both this way and by using
 @rhombus(repet_meta.unpack_generator). The result of
 @rhombus(repet_meta.unpack_list) can be convenient, but its use can be
 less efficient if a refied list is simply fed into another
 @rhombus(for)-like traversal.

@examples(
  ~eval: macro_eval
  ~defn:
    repet.macro 'enumed($from, $(sub :: repet_meta.Parsed))':
      ~all_stx stx
      let '($_, $expr, $depth, $use_depth, $_)':
        repet_meta.unpack_list(sub)
      let (_, si):
        let '$(p :: annot_meta.Parsed)' = 'List'
        annot_meta.unpack_predicate(p)
      repet_meta.pack_list(
        '($stx,
          for List (elem in $expr, i in $from ..): [i, elem],
          $depth,
          $use_depth,
          $si)'
      )
  ~repl:
    def [x, ...] = ["a", "b", "c"]
    [enumed(1, x), ...]
)

}


@doc(
  ~meta
  syntax_class repet_meta.Parsed:
    kind: ~group
    fields:
      group
  syntax_class repet_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class repet_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class repet_meta.NameStart:
    kind: ~group
    fields:
      name
      [head, ...]
      [tail, ...]
){

 Analogous to @rhombus(expr_meta.Parsed, ~stxclass), etc., but for
 repetition forms.

}


@doc(
  ~meta
  fun repet_meta.parse_dot(left :: Syntax, op_and_tail :: Group,
                           ~as_static: as_static = #false,
                           ~disable_generic: disable_generic = #false)
    :: (maybe(Syntax), maybe(Syntax))
){

 Analogous to @rhombus(expr_meta.parse_dot), but for repetitions.

 To select a specific dot provider, use @rhombus(repet_meta.unpack_list)
 plus @rhombus(repet_meta.pack_list) to adjust the static information
 of @rhombus(left), where @rhombus(statinfo_meta.dot_provider_key) is the
 relevant static-information key.

}


@(macro.close_eval(macro_eval))
