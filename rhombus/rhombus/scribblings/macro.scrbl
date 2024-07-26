#lang rhombus/scribble/manual

@title(~style: #'toc){Syntax Objects and Macros}

Rhombus's use of @tech{shrubbery notation} makes it possible to provide
convenient forms for matching and constructing syntactic terms. A
@tech{syntax object} encapsulates shrubbery structure---enriched with
binding information and metadata such as source locations---without
requiring that the shrubbery parses as an existing form. Pieces of a
syntax object can be extracted via pattern matching and then spliced
into new syntax-object constructions, which is the main work of defining
syntactic forms through macros.

@local_table_of_contents()

@include_section("syntax.scrbl")
@include_section("expr-macro.scrbl")
@include_section("defn-macro.scrbl")
@include_section("bind-macro.scrbl")
@include_section("annotation-vs-bind.scrbl")
@include_section("syntax-class.scrbl")
