#lang rhombus
import:
  rhombus/macro open
  scribble/base expose: #{include-section}

export:
  include_section

defn.macro 'include_section($(mod $: Term))':
  '$(parsed(['#{include-section}', mod]))'
