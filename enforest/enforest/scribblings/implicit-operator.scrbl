#lang rhombus/scribble/manual

@title(~tag: "implicit-ops"){Implicit Operators}

In much the same way that @rhombus(#%app) and @rhombus(#%datum) are
implicitly used in many Racket expressions, Rhombus enforestation needs
at least two implicit forms: an implicit prefix operator for a
non-identifier form by itself (somewhat like @rhombus(#%datum)), and an
implicit infix operator for the juxtaposition of a parsed form and
another form without a binary operator in between (somewhat like
@rhombus(#%app)). To help enforestation applications avoid a level of
indirection between those minimal implicit forms, however, enforestation
is parameterized over functions that select implicit prefix and infix
forms. The default selection function generates references to the
following forms:

@itemlist(

 @item{@rhombus(#%parens): implicit prefix for a parenthesized term that is not
   immediately after a parsed form}

 @item{@rhombus(#%call): implicit infix for a parsed form followed by a
   parenthesized term}

 @item{@rhombus(#%brackets): implicit prefix for a square-bracketed term that is not
   immediately after a parsed form}

 @item{@rhombus(#%index): implicit infix for a parsed form followed by a
   square-bracketed term}

 @item{@rhombus(#%braces): implicit prefix for a curly-braced term that is not
   immediately after a parsed form}

 @item{@rhombus(#%comp): implicit infix for a parsed form followed by a
   curly-braced term}

 @item{@rhombus(#%juxtapose): implicit infix for adjacent expressions with no
   operator between them when @rhombus(#%call), @rhombus(#%index), and @rhombus(#%comp) do not
   apply}

 @item{@rhombus(#%quotes): implicit prefix for a single-quoted term that is not
   immediately after a parsed form}

 @item{@rhombus(#%block): implicit prefix for a block (written with @litchar{:}) not
   immediately after a parsed form}

 @item{@rhombus(#%alts): implicit prefix for a sequence of alternatives
   (written with @litchar{|} notation) not immediately after a parsed form}

 @item{@rhombus(#%literal): implicit prefix for a literal, such as a number or
   boolean, not immediately after a parsed form}

)

In an expression context, a Rhombus language's @rhombus(#%call) implementation
most likely creates a function call, @rhombus(#%parens) most likely does
nothing for a single expression in parentheses (so parentheses can be
used for grouping) and might otherwise create a tuple value or return
multiple values, @rhombus(#%juxtapose) probably reports an error. Implicit
operators are likely to have the highest possible precedence and be
left-associative, but they are not constrained to those conventions by
the Rhombus expander. Implicit operators are likely to be implemented
using the macro operator protocol instead of the automatic operator
protocol.
