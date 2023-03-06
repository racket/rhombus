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

  Alias for the @rhombus(expr, ~space) @tech{space}.

}

@doc(
  ~nonterminal:
    id_macro_patterns: defn.macro
  defn.macro 'decl.macro $id_macro_patterns'
){

 Like @rhombus(defn.macro) but for defining a macro that can be used
 only in a module or interactive position --- the same places where
 @rhombus(meta) and @rhombus(submodule) are allowed, for example.

}

@doc(
  ~nonterminal:
    id_macro_patterns: defn.macro
  defn.macro 'decl.nestable_macro $id_macro_patterns'
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



@«macro.close_eval»(macro_eval)
