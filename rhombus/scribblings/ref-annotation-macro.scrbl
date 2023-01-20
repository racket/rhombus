#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Annotation Macros}

@doc(
  space.enforest annot
){

 The @tech{space} for bindings of identifiers and operators that can be
 used in annotation, such as after @rhombus(::).

}


@doc(
  defn.macro 'annot.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines an identifier or operator as an
 annotation form in the @rhombus(annot, ~space) @tech{space}.
 The result of the macro expansion can be a result
 created with @rhombus(annot_meta.pack_predicate).

@examples(
  ~eval: macro_eval
  annot.macro 'two_of($ann)':
    'matching(List(_ :: $ann, _ :: $ann))'
  [1, 2] :: two_of(Number)
  ~error: [1, 2, 3] :: two_of(Number)
  ~error: [1, "x"] :: two_of(Number)
)

}


@doc(
  fun annot_meta.pack_predicate(fun_stx:: Syntax,
                                statinfo_stx :: Syntax = '()') :: Syntax
){

 @provided_meta()

 Packs an expression for a predicate with static information into an
 annotation form as a syntax object. When the resulting annotation is
 applied to a value, it checks the value using the predicate, and it
 also associates the static information in @rhombus(statinfo_stx) with
 the value. The given @rhombus(statinfo_stx) is in unpacked form
 (i.e., @rhombus(statinfo_meta.pack) is applied automatically).

 See @secref("annotation-macro") for more explanation and for
 examples.

}


@doc(
  syntax_class annot_meta.Group:
    kind: ~group
    field parsed
  syntax_class annot_meta.AfterPrefixGroup(op_name):
    kind: ~group
    field parsed
    field [tail, ...]
  syntax_class annot_meta.AfterInfixGroup(op_name):
    kind: ~group
    field parsed
    field [tail, ...]
){

 @provided_meta()

 Analogous to @rhombus(expr_meta.Group, ~stxclass), etc., but for annotations.

}


@«macro.close_eval»(macro_eval)
