#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Export}

@doc(
  decl.macro 'export:
                $export_clause
                ...',
  
  grammar export_clause:
    $export_item
    $export_item:
      $modifier
      ...
    $modifier:
      $export_clause
      ...,

  grammar export_item:
    $identifier_or_operator
    $export,

  grammar identifier_or_operator:
    $identifier
    $operator,
){

 Exports from the enclosing module.

 An @rhombus(export_item) can be an identifier, operator, other export
 form, such as @rhombus(all_from, ~expmod).
 It can also be a sequence @rhombus(export_item)s within a
 group, since @rhombus(#{#%juxtapose}, ~expmod) is defined as an
 export form.

 Similar to @rhombus(import), a @rhombus(export_item) can be modified
 either through a subsequent block containing @rhombus(modifier)s or
 by a preceding @rhombus(modifier) with the @rhombus(export_item)s in
 a block. The latter order works only if the @rhombus(modifier) itself
 does not need a block.

}

@doc(
  expo.macro 'all_from($module_path)'
){

 Exports all bindings imported from @rhombus(module_path), where
 @rhombus(module_path) appears syntactically the same via
 @rhombus(import) or as the module's language.

}

@doc(
  expo.macro 'rename:
                $int_identifier_or_operator $$(@rhombus(as, ~expmod)) $ext_identifier_or_operator
                ...'
){

 For each @rhombus(as, ~expmod) group, exports
 @rhombus(int_identifier_or_operator) bound locally so that it's
 imported as @rhombus(ext_identifier_or_operator).

}

@doc(
  expo.macro 'names:
                $identifier_or_operator ...
                ...'
){

 Exports all @rhombus(identifier_or_operator)s.

 Most @rhombus(identifier_or_operator)s can be exported directly
 without using @rhombus(names, ~impmod), but the @rhombus(names, ~impmod)
 form disambiguates in the case of an @rhombus(identifier_or_operator) that is
 itself bound as an export form or modifier.

}

@doc(
  expo.macro '$export #{#%juxtapose} $export'
){

 Exports the union of bindings described by the two @rhombus(export)s.

 @see_implicit(@rhombus(#{#%juxtapose}, ~expmod), "an export", "export", ~is_infix: #true)

}

@doc(
  expo.macro 'as'
){

 Invalid by itself, and intended for use with @rhombus(rename, ~expmod).

}

@doc(
  expo.modifier 'except:
                  $export
                  ...'
){

 Modifies an export to remove the identifiers that would be exported
 by the @rhombus(export)s.

}

@doc(
  expo.modifier 'for_meta',
  expo.modifier 'for_meta $phase'
){

 Modifies exports to apply at @rhombus(phase) more than the enclosing
 context's phase, where @rhombus(phase) defaults to @rhombus(1).

}

@doc(
  expo.modifier 'for_label'
){

 Modifies exports to apply at the label phase.

}
