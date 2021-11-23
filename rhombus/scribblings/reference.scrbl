#lang scribble/rhombus/manual
@(import:
    rhombus/macro:
      no_prefix
    rhombus:
      no_prefix
      for_label)

@title{Core Rhombus Reference}

@docmodule[rhombus]

@doc[
  decl.macro '(import:
                 module_path:
                   modifier
                   ...
                 ...)
]{

 Imports into the enclosing module. Each @rhombus[module_path] should have
 the form...

}
