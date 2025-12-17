#lang rhombus/scribble/manual
@(import:
    "racket_names.rkt".racket_require
    meta_label:
      rhombus.import)

@(def Rhombus:
    @seclink("top", ~doc: ModulePath'lib("rhombus/scribblings/guide/rhombus-guide.scrbl")'){Rhombus})
@(def Racket:
    @seclink("top", ~doc: ModulePath'lib("scribblings/guide/guide.scrbl")'){Racket})

@title(~category: #'interop){Rhombus and Racket Interoperability}

@Rhombus is implemented on top of @Racket, and the two languages share a
module system and many data representations. Still, a direct
@rhombus(import) or @racket_require from a module in one language of a
module in the works only simple cases with functions or simple
datatypes. This document describes techniques and libraries for
interoperating between the two languages.

@table_of_contents()

@include_section("module.scrbl")
@include_section("data.scrbl")
@include_section("rhombus_to_racket.scrbl")
@include_section("racket_to_rhombus.scrbl")
