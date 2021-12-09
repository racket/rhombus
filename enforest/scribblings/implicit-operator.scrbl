#lang scribble/manual

@title[#:tag "implicit-ops"]{Implicit Operators}

In much the same way that @racket[#%app] and @racket[#%datum] are
implicitly used in many Racket expressions, Rhombus enforestation needs
at least two implicit forms: an implicit prefix operator for a
non-identifier form by itself (somewhat like @racket[#%datum]), and an
implicit infix operator for the juxtaposition of a parsed form and
another form without a binary operator in between (somewhat like
@racket[#%app]). To help enforestation applications avoid a level of
indirection between those minimal implicit forms, however, enforestation
is parameterized over functions that select implicit prefix and infix
forms. The default selection function generates references to the
following forms:

@itemlist[
          
 @item{@racket[#%tuple]: implicit prefix for a parenthesized term that is not
   immediately after a parsed form}

 @item{@racket[#%call]: implicit infix for a parsed form followed by a
   parenthesized term}

 @item{@racket[#%array]: implicit prefix for a square-bracketed term that is not
   immediately after a parsed form}

 @item{@racket[#%ref]: implicit infix for a parsed form followed by a
   square-bracketed term}

 @item{@racket[#%set]: implicit prefix for a curly-braced term that is not
   immediately after a parsed form}

 @item{@racket[#%comp]: implicit infix for a parsed form followed by a
   curly-braced term}

 @item{@racket[#%juxtapose]: implicit infix for adjacent expressions with no
   operator between them when @racket[#%call], @racket[#%ref], and @racket[#%comp] do not
   apply}

 @item{@racket[#%block]: implicit prefix for a block (written with @racket[:]) not
   immediately after a parsed form}

 @item{@racket[#%alts]: implicit prefix for a block of alternatives
   (written with @litchar{|} notation) not immediately after a parsed form}

 @item{@racket[#%literal]: implicit prefix for a literal, such as a number or
   boolean, not immediately after a parsed form}

 ]

In an expression context, a Rhombus language's @racket[#%call] implementation
most likely creates a function call, @racket[#%tuple] most likely does
nothing for a single expression in parentheses (so parentheses can be
used for grouping) and might otherwise create a tuple value or return
multiple values, @racket[#%juxtapose] probably reports an error. Implicit
operators are likely to have the highest possible precedence and be
left-associative, but they are not constrained to those conventions by
the Rhombus expander. Implicit operators are likely to be implemented
using the macro operator protocol instead of the automatic operator
protocol.
