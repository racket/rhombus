#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    lib("rhombus/private/module-path-meta.rkt").modpath)

@title{Export}

@doc(
  decl.nestable_macro 'export:
                         $export_clause
                         ...'

  decl.nestable_macro 'export $export_clause'

  grammar export_clause:
    $export_item
    $export_item:
      $modifier
      ...
    $modifier:
      $export_clause
      ...

  grammar export_item:
    $id_or_op
    #,(@rhombus(all_from, ~expo))($source)
    #,(@rhombus(rename, ~expo)) $rename_decl
    #,(@rhombus(names, ~expo)) $names_decl
    #,(@rhombus(all_defined, ~expo)) $all_defined_decl
    $export_item #,(@rhombus(#%juxtapose, ~expo)) $export_item
    $other_export

  grammar id_or_op:
    $id_name
    $op_name

  grammar modifier:
    #,(@rhombus(except, ~expo)) $except_decl
    #,(@rhombus(meta, ~expo)) $meta_decl
    #,(@rhombus(meta_label, ~expo))
    #,(@rhombus(only_space, ~expo)) $only_space_decl
    #,(@rhombus(except_space, ~expo)) $except_space_decl
    $other_modifier

){

 Exports from the enclosing module or namespace. An @rhombus(export) form with a
 single immediate @rhombus(export_clause) is shorthand for an
 @rhombus(export) form that has a block containing the single
 @rhombus(export_clause).

 An @rhombus(export_item) can be an identifier, operator, or other export
 form, such as @rhombus(all_from, ~expo).
 It can also be a sequence of @rhombus(export_item)s within a
 group, since @rhombus(#%juxtapose, ~expo) is defined as an
 export form.

 Similar to @rhombus(import), an @rhombus(export_item) can be modified
 either through a subsequent block containing @rhombus(modifier)s or
 by a preceding @rhombus(modifier) with the @rhombus(export_item)s in
 a block. The latter order works only if the @rhombus(modifier) itself
 does not need a block.

 An @rhombus(id_name) or @rhombus(op_name) export can be
 an immediate identifier or operator, or it can be dotted name, such as
 @rhombus(List.length). The last component of a dotted name is used as
 the export name. See @secref("namespaces") for information on
 @rhombus(id_name) and @rhombus(op_name).

}

@doc(
  ~nonterminal:
    module_path: import ~defn
  expo.macro 'all_from($module_path)'
  expo.macro 'all_from(#,(@rhombus(., ~expo)) $id_name)'
){

 With @rhombus(module_path), exports all bindings imported without a
 prefix from @rhombus(module_path), where @rhombus(module_path) appears
 the same via @rhombus(import). ``The same''
 means that the module paths are the same after some normalization: paths
 that use @rhombus(/)-separated indentifiers are converted to
 @rhombus(lib) forms, and in a @rhombus(lib) form, an implicit
 @filepath{.rhm} suffix is made explicit.

 With @rhombus(#,(@rhombus(., ~expo)) id_name), exports
 the content of the specified @tech{namespace} or module import (i.e.,
 the content that would be accessed with a prefix in the exporting
 context). See @secref("namespaces") for information on
 @rhombus(id_name).
}

@doc(
  ~nonterminal:
    int_id_or_op: block id_or_op
    ext_id_or_op: block id_or_op
  expo.macro 'rename:
                $int_id_or_op #,(@rhombus(as, ~expo)) $ext_id_or_op
                ...'
  expo.macro 'rename $int_id_or_op #,(@rhombus(as, ~expo)) $ext_id_or_op'
){

 For each @rhombus(as, ~expo) group, exports
 @rhombus(int_id_or_op) bound locally so that it's
 imported as @rhombus(ext_id_or_op).

}

@doc(
  expo.macro 'names:
                $id_or_op ...
                ...'
){

 Exports all @rhombus(id_or_op)s.

 Most @rhombus(id_or_op)s can be exported directly
 without using @rhombus(names, ~expo), but the @rhombus(names, ~expo)
 form disambiguates in the case of an @rhombus(id_or_op) that is
 itself bound as an export form or modifier.

}

@doc(
  expo.macro 'all_defined'
  expo.macro 'all_defined ~scope_like $id'
){

 Exports all identifiers defined within the enclosing module or
 namespace that could be referenced by a @rhombus(names) form using the
 same scopes as @rhombus(id) or (when no @rhombus(id) is provided) the
 @rhombus(all_defined, ~expo) identifier itself. Supplying
 @rhombus(~scope_like id) is particularly useful in the case of a
 macro-introduced @rhombus(all_defined) form.

 Imports using @rhombus(import) count as definitions only when the
 import is from a namespace. Imports from a module do no count as
 definitions.

}


@doc(
  ~nonterminal:
    export_item: export ~decl
  expo.macro '$export_item #%juxtapose $export_item'
){

 Exports the union of bindings described by the two @rhombus(export_item)s.

 @see_implicit(@rhombus(#%juxtapose, ~expo), "an export", "export", ~is_infix: #true)

}

@doc(
  expo.macro 'as'
){

 Invalid by itself, and intended for use with @rhombus(rename, ~expo).

}

@doc(
  ~nonterminal:
    export_item: export ~decl
  expo.modifier 'except $export_item'
  expo.modifier 'except:
                   $export_item
                   ...'
){

 Modifies an export to remove the bindings with the same external
 names as those that would be exported by the @rhombus(export_item)s.

}

@doc(
  expo.modifier 'meta',
  expo.modifier 'meta $phase'
){

 Modifies exports to apply at @rhombus(phase) more than the enclosing
 context's phase, where @rhombus(phase) defaults to @rhombus(1).

 This modifier is valid only immediately within a module, and not
 within @rhombus(namespace) forms.

}

@doc(
  expo.modifier 'meta_label'
){

 Modifies exports to apply at the label phase.

 This modifier is valid only immediately within a module, and not
 within @rhombus(namespace) forms.

}


@doc(
  expo.modifier 'only_space $id'
  expo.modifier 'only_space: $id ...'
  expo.modifier 'except_space $id'
  expo.modifier 'except_space: $id ...'
){

 Modifies an @rhombus(export) clause to include bindings only in the
 specifically listed @tech{spaces} or only in the spaces not specifically
 listed.

}


@doc(
  ~nonterminal:
    op_name: namespace ~defn
  expo.macro '$id_name . $id'
  expo.macro '$id_name . ($op)'
){

  In an export clause, @rhombus(., ~expo) can be used only to form an
  @rhombus(id_name) or @rhombus(op_name)
  as described for @rhombus(export). It can also be used to form an
  @rhombus(id_name) for @rhombus(all_from, ~expo).

}


@doc(
  ~nonterminal:
    collection_module_path: import ~defn
  modpath.macro '$id / $collection_module_path'
){

 Like the @rhombus(/, ~impo) operator for @rhombus(import) module
 paths, used for module paths in @rhombus(all_from, ~expo).

}


@doc(
  modpath.macro 'lib($string)'
){

 Like the @rhombus(lib, ~impo) form for @rhombus(import), used for
 module paths in @rhombus(all_from, ~expo).

}


@doc(
  modpath.macro 'file($string)'
){

 Like the @rhombus(file, ~impo) form for @rhombus(import), used for
 module paths in @rhombus(all_from, ~expo).

}


@doc(
  ~nonterminal:
    module_path: import ~defn
  modpath.macro '$module_path ! $id'
){

 Like the @rhombus(!, ~impo) operator for @rhombus(import) to access a
 submodule, used for module paths in @rhombus(all_from, ~expo).

}

@doc(
  modpath.macro 'self ! $id'
  modpath.macro 'parent'
){

 Like the @rhombus(self, ~impo) and @rhombus(parent, ~impo)
 @rhombus(import) forms, used for module paths in
 @rhombus(all_from, ~expo).

}
