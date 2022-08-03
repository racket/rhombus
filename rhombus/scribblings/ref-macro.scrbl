#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~style: symbol(toc)){Macros}

@docmodule(rhombus/macro)

Simple pattern-based expression macros can be written using
@rhombus(def) without imorting any additional libraries besides
@rhombusmodname(rhombus), but implementing others forms of macros requires
using the @rhombusmodname(rhombus/macro) module (usually with no prefix).

The @rhombusmodname(rhombus/macro) module provides bindings like
@rhombus(expr.macro), and it also re-exports all of
@rhombusmodname(rhombus) @rhombus(for_meta) for use in compile-time
expressions.

@local_table_of_contents()

@include_section("ref-expr-macro.scrbl")
@include_section("ref-defn-macro.scrbl")
