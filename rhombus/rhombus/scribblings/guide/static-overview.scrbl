#lang rhombus/scribble/manual

@title(~style: #'toc, ~tag: "static-info"){Static Information, Binding, and Annotation}

@tech{Static information} is closely related to @tech{annotations}, because annotations
are often the source of static information, either applied to an
expression or associated with a binding. @tech{Binding}, in turn, is closely
related to static information, because annotations are often applied at
bindings.

@local_table_of_contents()

@include_section("static-info.scrbl")
@include_section("static-info-keys.scrbl")
@include_section("static-info-rule.scrbl")
@include_section("annotation-macro.scrbl")
@include_section("bind-macro-protocol.scrbl")
