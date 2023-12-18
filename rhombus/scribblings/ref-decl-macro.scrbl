#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@title{Declaration Macros}

@doc(
  space.transform decl
){

 The @tech{space} for bindings of identifiers that can be used in
 declaration and nestable-declaration positions.

}

@doc(
  def decl_meta.space :: SpaceMeta
){

@provided_meta()

 A compile-time value that identifies the same space as
 @rhombus(decl, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
  defn.macro 'decl.macro $prefix_macro_patterns'
){

 Like @rhombus(defn.macro) but for defining a macro that can be used
 only in a module or interactive position --- the same places where
 @rhombus(meta) and @rhombus(module) are allowed, for example.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
  defn.macro 'decl.nestable_macro $prefix_macro_patterns'
){

 Like @rhombus(defn.macro), but for forms that can also be used in
 namespaces that are witin a module or interactive position --- the same
 places where @rhombus(export) is allowed, for example.

}

@doc(
  syntax_class decl_meta.Group:
    kind: ~group
  syntax_class decl_meta.NestableGroup:
    kind: ~group
){

 @provided_meta()

 Like @rhombus(defn_meta.Group, ~stxclass), but for declarations and
 nestable declarations. The @rhombus(decl_meta.Group, ~stxclass) syntax
 class matches all groups that
 @rhombus(decl_meta.NestableGroup, ~stxclass) matches, plus ones that
 cannot be nested.

}


@doc(
  fun decl_meta.pack_s_exp(tree :: Any) :: Syntax
){

@provided_meta()

 Similar to @rhombus(expr_meta.pack_s_exp), but for declarations.


}


@(macro.close_eval(macro_eval))
