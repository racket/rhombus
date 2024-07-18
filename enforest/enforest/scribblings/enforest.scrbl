#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Enforestation with Macro Expansion}

@seclink(~doc: shrubbery_doc, "top"){Shrubbery notation} specifies
how to parse a sequence of characters into a coarse-grained block
structure, but it leaves the interpretation of that block structure to
another layer of parsing---not to mention more fine-grained grouping in
between lexing and block structure. The @filepath{enforest} collection
is an adaptation of the Racket and Honu parsing techniques of
@emph{expansion} and @emph{enforestation} targeted to shrubbery
notation.

For brevity, we call this parsing layer @deftech{Rhombus expansion},
even though it does not define a full candidate Rhombus language, and
although it's in many ways independent of a specific language. That's
similar to referring to @emph{Racket expansion}, by which we do not
necessarily mean something involving @rhombus(#,(hash_lang()) #,(@rhombuslangname(racket))).

@table_of_contents()

@include_section("motivation.scrbl")
@include_section("syntactic-categories.scrbl")
@include_section("hierarchical-naming.scrbl")
@include_section("transformer.scrbl")
@include_section("precedence.scrbl")
@include_section("implicit-operator.scrbl")
@include_section("macro-protocol.scrbl")
@include_section("enforest-algorithm.scrbl")
@include_section("api.scrbl")
@include_section("example.scrbl")
