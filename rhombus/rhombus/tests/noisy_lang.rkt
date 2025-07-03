#lang rhombus
import:
  rhombus/meta open

export:
  all_from(rhombus):
    except #%module_block
  rename:
    module_block as #%module_block

decl.macro 'module_block: $form; ...':
  '#%module_block:
     println($(form.to_source_string()))
     ...
     $form
     ...'

module reader ~lang rhombus/reader:
  ~lang: "noisy_lang.rhm"
