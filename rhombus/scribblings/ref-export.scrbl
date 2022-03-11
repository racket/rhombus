#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Export}

@doc[
  decl.macro 'export:
                $export_item ... :
                  $export_modifier
                  ...
                ...',
  
  grammar export_item:
    $identifier_or_operator
    all_from($module_path)
    all_in($identifier)
    names: $identifier_or_operator ...,

  grammar identifier_or_operator:
    $identifier
    $operator,
]{

 Exports from the enclosing module.

}

