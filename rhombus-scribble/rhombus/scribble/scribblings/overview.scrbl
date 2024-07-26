#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def shrubbery_doc: ModulePath 'lib("shrubbery/scribblings/shrubbery.scrbl")')

@title(~tag: "overview"){Overview}

The @rhombuslangname(rhombus/scribble) language is like the
@rhombuslangname(rhombus) language, but with three key differences:

@itemlist(

 @item{A
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
 body starts in text mode as if inside @litchar("@{}") (see
 @secref(~doc: shrubbery_doc, "at-notation")), which means that
 @litchar("@") escapes must be used to write Rhombus definitions or
 expressions for implementing a document.}

 @item{Since it starts in text mode, the body of a
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
 produces a mixture of string literals and expressions. Those results are
 gathered together and decoded to form a document.}

 @item{The @rhombuslangname(rhombus/scribble) module provides all of the
 binding of @rhombuslangname(rhombus) plus a group of bindings for writing
 documents. Those bindings are described in @secref("base"). The
 @rhombuslangname(rhombus/scribble/manual) module provides even more
 bindings as described in @secref("manual").}

)

Functions and forms added by @rhombuslangname(rhombus/scribble) to
@rhombuslangname(rhombus) are primarily intended to be used with
@litchar("@") notation. For example, the @rhombus(italic) function could
be called as @tt{italic(["Hello"])}, but the intended use syntax is
@tt|{@italic{Hello}}|. Some functions accept extra argument, especially
keyword-based optional arguments, and those can be supplied in
parentheses before a final text argument written with @litchar{{}}. Some
functions, such as @rhombus(secref), do not have text-content arguments,
and so they are @emph{not} meant to be used with @litchar("{}")
arguments.