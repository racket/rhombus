#lang rhombus/scribble/manual

@title(~style: #'toc){Shrubbery APIs}

Shrubbery APIs are generally intended for use from Racket, while Rhombus
operations natively work on values that are internally represented in
Shrubbery form. See also @rhombusmodname(rhombus/shrubbery) for reading
Shrubbery forms in Rhombus.

@local_table_of_contents()

@include_section("parse.scrbl")
@include_section("raw-text.scrbl")
@include_section("print.scrbl")
@include_section("write.scrbl")
@include_section("tool.scrbl")
@include_section("variant.scrbl")
