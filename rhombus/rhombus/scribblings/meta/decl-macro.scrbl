#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval = macro.make_macro_eval())

@title{Declaration Macros}

@doc(
  space.transform decl
){

 The @tech{space} for bindings of identifiers that can be used in
 @tech{declaration} and @tech{nestable-declaration} positions.

}

@doc(
  ~meta
  def decl_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(decl, ~space). See also @rhombus(SpaceMeta, ~annot).

 Unlike @rhombus(defn.macro), the @rhombus(~name_prefix) option is not
 supported, because a declaration cannot be nested in a namespace.

}


@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
  defn.macro 'decl.macro $prefix_macro_patterns'
){

 Like @rhombus(defn.macro) but for defining a macro in
 @deftech{declaration} positions, which are in
 in a module or interactive position---the same places where
 @rhombus(meta) and @rhombus(module) are allowed, for example.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
  defn.macro 'decl.nestable_macro $prefix_macro_patterns'
){

 Like @rhombus(defn.macro), but for forms that can also be used in
 @deftech{nestable-declaration} positions, which includes
 namespaces that are within a module or interactive position---the same
 places where @rhombus(export) is allowed, for example.

}

@doc(
  ~meta
  syntax_class decl_meta.Group:
    kind: ~group
  syntax_class decl_meta.NestableGroup:
    kind: ~group
){

 Like @rhombus(defn_meta.Group, ~stxclass), but for @tech{declarations} and
 @tech{nestable declarations}. The @rhombus(decl_meta.Group, ~stxclass) syntax
 class matches all groups that
 @rhombus(decl_meta.NestableGroup, ~stxclass) matches, plus ones that
 cannot be nested.

}


@doc(
  ~meta
  fun decl_meta.pack_s_exp(tree :: Any) :: Syntax
){

 Similar to @rhombus(expr_meta.pack_s_exp), but for declarations.


}


@(macro.close_eval(macro_eval))
