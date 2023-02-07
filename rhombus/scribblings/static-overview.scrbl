#lang scribble/rhombus/manual

@title(~style: #'toc, ~tag: "static-info"){Static Information and Binding}

A binding or an expression can have associated @deftech{static
 information} that is used to enable, reject, or resolve certain
expression forms. For example, an expression can be used to the left of
a @rhombus(.) only when it has static information to specify how a field
name after @rhombus(.) resolves to an field accessor. See the
@secref("annotation") for an introduction to static information and its
role.

Static information is closely related to annotation, because annotations
are often the source of static information, either applied to an
expression or associated with a binding. Binding, in turn, is closely
related to static information because annotations are often applied at
bindings.

@local_table_of_contents()

@include_section("static-info.scrbl")
@include_section("static-info-rule.scrbl")
@include_section("annotation-macro.scrbl")
@include_section("bind-macro-protocol.scrbl")
