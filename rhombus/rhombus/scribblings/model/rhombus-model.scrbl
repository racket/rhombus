#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(
  ~category: [#'core, 16],
){Rhombus Language Model}

This document defines the evaluation and syntax concepts of the Rhombus
language.

@itemlist(

 @item{For a general overview of the language, see @docref(guide_doc).}

 @item{For documentation on the bindings that implement the concepts
 described here, see @docref(ref_doc) and @docref(meta_doc).}

 @item{For even more, see @top_ref().}

)

@table_of_contents()

@include_section("eval-model.scrbl")
@include_section("syntax-model.scrbl")
@include_section("static-overview.scrbl")
@include_section("io-model.scrbl")
