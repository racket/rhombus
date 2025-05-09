#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def shrubbery_notation = @shrubref("top"))

@title{Notation}

Like most languages, Rhombus syntax builds on a set of rules for parsing
characters into @deftech{tokens}. Unlike most languages---but like Lisp,
Scheme, and Racket---Rhombus syntax uses an additional layer of rules
for grouping and nesting tokens. For languages in the Lisp family, the
intermediate structure is @deftech{S-expression notation}, which gives
Lisp its parenthesized, prefix notation. For Rhombus, the intermediate
structure is @deftech{shrubbery notation}, which is designed to
support traditional infix operators and rely on line breaks
and indentation for grouping and nesting.

See @(shrubbery_notation) for complete details, but the basics (such as
numbers and identifiers) look like many other languages, and here are a
few extra hints:

@itemlist(

 @item{Shrubbery notation is whitespace-sensitive, so line breaks and
 indentation matter.}

 @item{The @colon and @vbar tokens are not operators, but instead
 determine program structure along with line breaks and indentation.
 Multi-character tokens with @colon and @vbar can be operators, such as
 @rhombus(::), @rhombus(:~), or @rhombus(||).}

 @item{Keywords start with @litchar{~}, and they are a distinct
 syntactic category from identifiers and operators.}

 @item{Single-quote marks @quotes are used for quoting code (i.e.,
 shrubbery forms), not strings.}

 @item{Indentation rules still apply within @parens, @brackets, @braces,
 and @quotes.}

)
