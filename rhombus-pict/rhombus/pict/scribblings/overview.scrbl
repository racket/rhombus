#lang rhombus/scribble/manual
@(import:
    meta_label:
      pict.Pict)

@title(~style: #'toc){Overview}

The @rhombus(Pict, ~annot) datatype encompasses both @tech{static picts}
and @tech{animated picts}.

@local_table_of_contents()

@include_section("static-overview.scrbl")
@include_section("animated-overview.scrbl")
@include_section("nothing-overview.scrbl")
@include_section("identity.scrbl")
