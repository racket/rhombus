#lang rhombus/scribble/manual

@title(~category: #'#{core}){Rhombus FFI: Foreign-Function Interface}

@docmodule(~open, ffi)

The @rhombusmodname(ffi) library enables direct use of foreign libraries
that have a C-based API—without writing any new C code. From the Rhombus
perspective, functions and data with a C-based API are
@defterm{foreign}, hence the term @defterm{foreign interface}.
Furthermore, since most APIs consist mostly of functions, the foreign
interface is sometimes called a @defterm{foreign function interface},
abbreviated @defterm{FFI}.

@table_of_contents()

@include_section("overview.scrbl")
@include_section("api.scrbl")
@include_section("more.scrbl")
