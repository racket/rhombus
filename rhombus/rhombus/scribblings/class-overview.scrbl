#lang scribble/rhombus/manual

@title(~style: #'toc, ~tag: "class-overview"){Classes and Interfaces}

@Secref("classes_and_patterns") shows the basic syntax of
@rhombus(class) for creating a new datatype. The @rhombus(class) form
also supports a body block (using @litchar{:}) immediately after the
parenthesized field sequence. The body block supplies clauses that
further customize a class, including making it a subclass of an existing
class, adding methods to the class, and customizing its constructor. The
@rhombus(interface) form is similar to @rhombus(class), but an interface
has only methods and can be implemented by classes.

@local_table_of_contents()

@include_section("subclass.scrbl")
@include_section("interface.scrbl")
@include_section("method.scrbl")
@include_section("property.scrbl")
@include_section("constructor.scrbl")
@include_section("custom-binding.scrbl")
@include_section("private-method.scrbl")
@include_section("private-implement.scrbl")
@include_section("class-namespace.scrbl")
@include_section("class-together.scrbl")
