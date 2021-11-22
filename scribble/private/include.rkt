#lang rhombus
import:
  rhombus/macro: no_prefix
  scribble/base: expose: #{include-section}

export:
  include_section

defn.macro '(include_section($(mod $: Term))):
  '(:
      $(parsed(['#{include-section}, mod])))
