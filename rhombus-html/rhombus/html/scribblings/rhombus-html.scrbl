#lang rhombus/scribble/manual

@title(~category: #'#{parsing-library}){HTML Reading and Writing}

@docmodule(html)

The @rhombusmodname(html) library provides functions to read and write
HTML (i.e., HTML5) documents.

The related @rhombusmodname(html/page, ~indirect) library
provides syntax for programmatic construction of new HTML pages with CSS
styling.

@local_table_of_contents()

@include_section("class.scrbl")
@include_section("read-write.scrbl")
@include_section("syntax.scrbl")
