#lang rhombus/scribble/manual
@(import:
    "grammar.rhm" open
    "grammar-s-exp.rkt":
      expose: shrubbery_s_expression_grammar)

@title(~category: #'core){Shrubbery Notation}

Shrubbery notation is a set of text-level conventions that build toward a
full programming language, such as
@seclink(~doc: ModulePath 'lib("rhombus/scribblings/main/rhombus.scrbl")', "top"){Rhombus}.
The notation is line- and indentation-sensitive, and it is intended to partially group
input, but leave further parsing to another layer, especially
@seclink(~doc: ModulePath 'lib("enforest/scribblings/enforest.scrbl")', "top"){enforestation}.
The parsed form of a shrubbery imposes grouping to ensure that further
parsing is consistent with the shrubbery's lines and indentation.

@table_of_contents()

@include_section("example.scrbl")
@include_section("spec.scrbl")
@include_section("parsed-representation.scrbl")
@include_section("language.scrbl")
@include_section("api.scrbl")
@include_section("meta.scrbl")
@include_section("editor.scrbl")
