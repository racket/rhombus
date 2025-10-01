#lang rhombus

import:
  rhombus/meta open

module reader ~lang rhombus/reader:
  ~lang parent

export:
  all_from(rhombus):
    except #%module_block
  rename:
    module_block as #%module_block

decl.macro 'module_block:
              ~reflect_id $id
              $form; ...':
  '#%module_block:
     def $id = Syntax.literal($form,
                              ...)
     $form
     ...'
