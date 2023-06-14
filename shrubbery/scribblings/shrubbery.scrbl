#lang scribble/rhombus/manual
@(import:
    "grammar.rhm" open
    "grammar-s-exp.rkt":
      expose: shrubbery_s_expression_grammar)

@title{Shrubbery Notation}

Shrubbery notation is a set of text-level convention that build toward a
full programming language, such as
@seclink(~doc: [#'lib, "rhombus/scribblings/rhombus.scrbl"], "top"){Rhombus}.
The notation is line- and indentation-sensitive, and it is intended to partially group
input, but leave further parsing to another layer, especially
@seclink(~doc: [#'lib, "enforest/scribblings/enforest.scrbl"], "top"){enforestation}.
The parsed form of a shrubbery imposes grouping to ensure that further
parsing is consistent with the shrubbery's lines and indentation.

@table_of_contents()

@include_section("example.scrbl")
@include_section("group-and-block.scrbl")
@include_section("token-parsing.scrbl")
@include_section("at-parsing.scrbl")
@include_section("parsed-representation.scrbl")
@include_section("language.scrbl")
@include_section("meta.scrbl")
@include_section("tool.scrbl")
