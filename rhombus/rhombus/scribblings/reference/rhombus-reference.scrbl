#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc){Rhombus Reference}

This document defines the main Rhombus language bindings.

@itemlist(

 @item{For a general overview of the language, see @docref(guide_doc).}

 @item{For more bindings that support metaprograming, see @docref(meta_doc).}

 @item{For more documentation, see @docref(getting_started_doc).}

)

@docmodule(~lang,
           ~use_sources: lib("rhombus/private/amalgam.rkt")!core
                         lib("rhombus/private/amalgam.rkt")!#{core-macro}
                         lib("rhombus/private/amalgam.rkt")!#{core-meta}
                         lib("rhombus/private/amalgam.rkt")!#{core-derived},
           rhombus)

@margin_note_block{Unless otherwise specified at the start of a section, the
 bindings described in this manual are exported by the
 @rhombuslangname(rhombus) language.}

@table_of_contents()

@include_section("notation.scrbl")

@include_section("context.scrbl")
@include_section("name.scrbl")
@include_section("fun_oper.scrbl")
@include_section("branch.scrbl")
@include_section("object.scrbl")
@include_section("data.scrbl")
@include_section("collection.scrbl")
@include_section("protocol.scrbl")
@include_section("control.scrbl")
@include_section("code.scrbl")
@include_section("format.scrbl")
@include_section("io.scrbl")
@include_section("os.scrbl")
@include_section("concurrency.scrbl")
@include_section("security.scrbl")
@include_section("runtime.scrbl")
