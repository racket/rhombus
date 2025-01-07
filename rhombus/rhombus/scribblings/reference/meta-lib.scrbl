#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc, ~tag: "meta_and_macros"){Meta and Macros}

@docmodule(~use_sources: lib("rhombus/private/amalgam.rkt")!#{core-meta},
           rhombus/meta)

@docmodule(~lang, ~no_declare, rhombus/and_meta)

Simple pattern-based expression macros can be written using
@rhombus(macro) without importing any additional libraries besides
@rhombuslangname(rhombus), but implementing others forms of macros requires
using the @rhombusmodname(rhombus/meta) module (usually with no prefix).

The @rhombusmodname(rhombus/meta) module provides bindings like
@rhombus(defn.macro), @rhombus(expr.macro), and @rhombus(bind.macro). It
it also re-exports most of @rhombuslangname(rhombus) as @rhombus(meta, ~impo)
for use in compile-time expressions, but it omits bindings from
@rhombuslangname(rhombus) that bridge to meta contexts: @rhombus(meta),
@rhombus(macro), @rhombus(binding, ~class_clause), etc. Explicitly
import @rhombuslangname(rhombus) as @rhombus(meta, ~impo) to access the
omitted bindings.

The @rhombuslangname(rhombus/and_meta) module provides all of the
bindings from both @rhombuslangname(rhombus) and
@rhombusmodname(rhombus/meta). It's intended for use as a language---an
alternative to starting with @rhombuslangname(rhombus) and importing
@rhombusmodname(rhombus/meta).

@local_table_of_contents()

@include_section("meta.scrbl")
@include_section("space.scrbl")
@include_section("namespace-macro.scrbl")
@include_section("defn-macro.scrbl")
@include_section("decl-macro.scrbl")
@include_section("expr-macro.scrbl")
@include_section("assign-macro.scrbl")
@include_section("bind-macro.scrbl")
@include_section("annotation-macro.scrbl")
@include_section("static-info.scrbl")
@include_section("dot-provider.scrbl")
@include_section("repet-macro.scrbl")
@include_section("operator-order.scrbl")
@include_section("macro-more.scrbl")
@include_section("stxobj-meta.scrbl")
@include_section("syntax-parameter.scrbl")
@include_section("stxobj-map.scrbl")
@include_section("doc.scrbl")
