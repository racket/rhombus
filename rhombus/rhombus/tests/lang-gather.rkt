#lang rhombus/and_meta

import:
  rhombus
  rhombus/meta open

module reader ~lang rhombus/reader:
  ~lang parent

export:
  all_from(.rhombus):
    except #%module_block
  rename:
    gather_module_block as #%module_block

decl.macro 'gather_module_block:
              ~gather_id $id
              $form; ...':
  '«module_block:
     ~effect gather
     def mutable $id :~ List = []
     expr.macro 'gather $('$')(e :: expr_meta.Parsed) $('$')()':
       '$id := $id ++ [$('$')e]'
     $form
     ...»'
