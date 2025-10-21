#lang rhombus/scribble

@(def guide_doc = ModulePath'lib("rhombus/scribblings/rhombus.scrbl")')

@title{Rhombus Metaprogramming Tutorial}

This tutorial is about syntax and macros in @docref(guide_doc). It's
intended for readers who are familiar with environment-passing
interpreters, but only because interpreters serve as a stand-in for
useful applications, and interpreters are @emph{not} the kind of
metaprogramming that the tutorial is about. The tutorial doesn't assume
that you've used Rhombus before, but it also doesn't dwell much on the
everyday-programming part of the language. Instead, the tutorial aims to
introduce the nuts and bolts of Rhombus's approach to parsing, syntactic
extension, and language creation.

@table_of_contents()

@include_section("setup.scrbl")
@include_section("crash_course.scrbl")
@include_section("shrubbery.scrbl")
@include_section("parse.scrbl")
@include_section("enforest.scrbl")
@include_section("space.scrbl")
@include_section("implicit.scrbl")
@include_section("language.scrbl")
