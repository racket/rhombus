#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/unsafe open)

@title{Unsafe Mode}

@docmodule(~use_sources: lib("rhombus/unsafe.rhm"),
           rhombus/unsafe)

@doc(
  decl.macro 'use_unsafe'
){

 When included a module, causes the module to be compiled in unsafe
 mode, which may skip annotation checks on primitive operation to
 potentially improve performance. If a skipped check would have failed,
 however, the resulting behavior is unspecified and may lead to a crash
 or unpredictable behavior either immediately or at an arbitrarily later
 time.

}
