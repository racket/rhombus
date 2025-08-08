#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     shrubbery/write
                     shrubbery/parse))

@title[#:tag "write"]{Writing Shrubbery Notation}

@defmodule[shrubbery/write]{
 Unlike the functions of @racketmodname[shrubbery/print],
 functions from @racketmodname[shrubbery/write] output a
 shrubbery representation without using raw-text properties or
 other metadata.}

@defproc[(write-shrubbery [v any/c]
                          [port output-port? (current-output-port)]
                          [#:pretty? pretty? any/c #f]
                          [#:width width (or/c exact-nonnegative-integer?) #f]
                          [#:armor? armor? any/c #f]
                          [#:prefer-multiline? prefer-multiline? any/c #f])
         void?]{

 Prints @racket[v], which must be a valid S-expression representation of
 a shrubbery (see @secref["parsed-rep"]). Reading the
 printed form back in with @racket[parse-all] produces the same
 S-expression representation as @racket[v]. @tech{Raw text} properties
 are not used.

 The default mode with @racket[pretty?] as @racket[#false] prints in a
 simple and relatively fast way (compared to
 @racket[pretty-shrubbery]). When @racket[pretty?] is a true value,
 single-line output is preferred if @racket[width] is @racket[#false],
 otherwise it is preferred only when the line fits with @racket[width]
 columns. Use @racket[pretty-shrubbery] to gain more
 control over line choices when printing.

 If @racket[pretty?] is @racket[#false] or @racket[armor?] is a true
 value, then the printed form is @seclink["guillemet"]{line- and
  column-insensitive}. If @racket[pretty?] is a true value, @racket[armor?]
 is @racket[#f], and @racket[prefer-multiline?] is a true value, then
 line breaks are used instead of @litchar{«} and @litchar{»}.

 Note that @racket[write-shrubbery] expects an S-expression, not a
 syntax object, so it cannot use @seclink["raw-text"]{raw text properties}.
 See also @racket[shrubbery-syntax->string].

}

@defproc[(pretty-shrubbery [v any/c]
                           [#:armor? armor? any/c #f]
                           [#:prefer-multiline? prefer-multiline? any/c #f])
         any/c]{

 Produces a description of how to print @racket[v] with newlines and
 indentation. The printed form is @seclink["guillemet"]{line- and
  column-insensitive} if @racket[armor?] is a true value. If @racket[armor?]
 is @racket[#f] and @racket[prefer-multiline?] is a true value, then
 line breaks are used instead of @litchar{«} and @litchar{»}.

 The description is an S-expression DAG (directed acyclic graph) that
 represents pretty-printing instructions and alternatives:

 @itemlist[

 @item{@racket[_string] or @racket[_bytes]: print literally.}

 @item{@racket['nl]: print a newline followed by spaces corresponding to
   the current indentation.}

 @item{@racket[`(seq ,_doc ...)]: print each @racket[_doc] in sequence,
   each with the same indentation.}

 @item{@racket[`(nest ,_n ,_doc)]: print @racket[_doc] with the current
   indentation increased by @racket[_n].}

 @item{@racket[`(align ,_doc)]: print @racket[_doc] with the current
   indentation set to the current output column.}

 @item{@racket[`(or ,_doc ,_doc)]: print either @racket[_doc]; if
   @racket[prefer-multiline?] is @racket[#f], always taking the first
   @racket[_doc] in an @racket['or] will produce single-line output,
   while always taking the second @racket[_doc] will print a maximal
   number of lines.}

]

 The description can be a DAG because @racket['or] alternatives might
 have components in common. In the worst case, a tree view of the
 instructions can be exponentially larger than the DAG representation.

}
