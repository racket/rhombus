#lang rhombus/scribble/manual
@(import:
    "racket_names.rkt".racket_require
    meta_label:
      rhombus.import)

@title(~style: #'toc, ~tag: "racket-to-rhombus"){In Racket: Using Rhombus}

See @secref("module") for general information about Racket and Rhombus
modules, and see @secref("data") for general information about Racket
and Rhombus values.

@table_of_contents()

@include_section("parse.scrbl")
@include_section("dot.scrbl")
@include_section("dynamic-require.scrbl")
