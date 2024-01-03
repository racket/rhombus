#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@title{Repetition Macros}

@doc(
  space.transform repet
){

 The @tech{space} for bindings of @tech{repetitions}.

}


@doc(
  def repet_meta.space :: SpaceMeta
){

@provided_meta()

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
      repet_meta.pack_list('($self, n, PairList[0, 1, 2], 1, 0, (), #true)')
  ~repl:
    [nat3, ...]
)

}


@doc(
  fun repet_meta.pack_list(stx :: Syntax) :: Syntax
){

@provided_meta()

 Packs the implementation of a repetition form to serve as the result of
 expanding a repetition macro, where elements of the repetition are
 provided through a list.

 The syntax object @rhombus(stx) must have the following shape:

 @rhombusblock(
  '(#,(@rhombus(source_form, ~var)),
    #,(@rhombus(name_id, ~var)),
    #,(@rhombus(pair_list_expr, ~var)),
    #,(@rhombus(total_depth, ~var)),
    #,(@rhombus(use_depth, ~var)),
    ((#,(@rhombus(static_key, ~var)), #,(@rhombus(static_value, ~var))), ...),
    #,(@rhombus(is_immediate, ~var)))'
   )

 The @rhombus(source_form, ~var) group is used for error repoting to
 show the repetition, such as when the repeition is used at the wrong
 depth.

 The @rhombus(name_id, ~var) term is for error reporting and reflection
 in the sense that it is used as the inferred name for an element of the
 repetition, in case such a name is relevant.

 The @rhombus(pair_list_expr, ~var) expression produces a @rhombus(PairList, ~annot) that contains
 the elements of the repetition. Lists must be nested according to the
 repeition's depth: a list of elements for depth 1, a list of element
 lists for depth 2, and so on.

 The @rhombus(total_depth, ~var) integer specifies the depth of the
 original repetition, while @rhombus(use_depth, ~var) specifies how much
 of the original depth has already been extracted; that is, the
 difference @rhombus(total_depth, ~var) minus @rhombus(use_depth, ~var)
 indicates the depth of the list nesting that is produced by
 @rhombus(list_expr, ~var). A non-zero @rhombus(use_depth, ~var) might be
 relevant to reporting the use of a repetition at the wrong depth in
 terms of the original form's depth.

 The @rhombus(static_key, ~var)--@rhombus(static_value, ~var) pairs
 describe ``upward'' static information for inidvidual elements of the
 repeition. This information is automatically packed via
 @rhombus(statinfo_meta.pack).

 The @rhombus(is_immediate, ~var) component must be a literal boolean. A
 @rhombus(#true) indicates that @rhombus(list_expr, ~var) can be efficiently
 evaluated multiple times, while @rhombus(#false) indicates that
 @rhombus(list_expr, ~var) should be lifted out of enclosing repetitions
 to avoid evaluating it multiple times.

}

@doc(
  fun repet_meta.unpack_list(stx :: Syntax) :: Syntax
){

@provided_meta()

 The inverse of @rhombus(repet_meta.pack_list), which is useful for unpacking
 information about the expansion of nested repetation forms.

@examples(
  ~eval: macro_eval
  ~defn:
    repet.macro 'enum($from, $(sub :: repet_meta.Parsed))':
      ~op_stx self
      let '($_, $name, $expr, $depth, $use_depth, $_, $_)':
        repet_meta.unpack_list(sub)
      let (_, si):
        let '$(p :: annot_meta.Parsed)' = 'List'
        annot_meta.unpack_predicate(p)
      repet_meta.pack_list(
        '($self(),
          $name,
          for PairList (elem: $expr, i: $from ..): [i, elem],
          $depth,
          $use_depth,
          $si,
          #false)'
      )
  ~repl:
    def [x, ...] = ["a", "b", "c"]
    [enum(1, x), ...]
)

}


@(macro.close_eval(macro_eval))
