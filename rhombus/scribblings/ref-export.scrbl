#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Export}

@doc[
  decl.macro '(export:
                 export_item ... :
                   export_modifier
                   ...
                 ...),
  
  grammar export_item:
    identifier-or-operator
    all_from(module_path)
    all_in(identifier)
    names: identifier-or-operator ...,

  grammar identifier-or-operator:
    identifier
    operator,
]{

 Exports from the enclosing module.

}

