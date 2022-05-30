#lang scribble/rhombus/manual
@(import: "common.rhm": open)

@title{Macro protocol}

When an operator's mapping indicates a macro protocol, then the
operator's transformer procedure is called with a parsed left-hand
argument (in the case of an infix operator) and all of the remaining
terms in the operator's group starting with the operator. The
transformer procedure must return two values: the parsed form, which
normally incorporates the left-hand argument plus some consumed
additional terms, and the remaining terms that were not consumed. The
Rhombus expander does not check that the second result is a tail of the
original remaining terms, so a transformer could replace or rearrange
them, but that's probably not a good idea. The invocation of a
transformer for an implicit operator includes a synthesized term for the
implicit operator.

The enforestation process recognizes a superset of S-expressions
compared to those used to represent shrubberies. Specifically, it
recognizes a two-element list term that starts with the symbol
@racket_q_parsed. The S-expression after @racket_q_parsed is treated
as an already-parsed term and need not conform to the shrubbery
grammar's representation. Since the list starting @racket_q_parsed
itself has no shrubbery representation, it's conveniently opaque to a
shrubbery layer of patterning matching. For example, in the earlier
example implementing the @rhombus[->] macro infix operator,

@(rhombusblock:
    expr.macro '($x -> $y $tail ...)':
      values('($x . $y)', tail)
  )

the macro transformer receives an syntax-object representing the
already-parsed left-hand argument @rhombus[x] as a @racket_q_parsed
list. The macro therefore has no way to pull the expression apart,
inspect it, or rearrange it. Of course, such facilities could be made
available to the macro transformer in lower-level form. Meanwhile,
@rhombus[y] and @rhombus[tail] are likely unparsed terms, that can be
inspected—although it's possible that some other macro constructs a
@rhombus[->] expression using already-parsed terms, in which case they
are similarly opaque to the @rhombus[->] transformer.

For binding-operator macros, @Rhombus includes @rhombus[bind.unpack] to
expose certain pieces of a binding's implementation, which allows the
macro to compose or adjust other binding expansions. New binding pieces
can be put back together into a parsed form using @rhombus[bind.pack].

Some contexts may oblige a macro transformer to consume all of the
remaining terms in a group. For example, a definition or declaration
context based on prefix identifiers like @rhombus[import],
@rhombus[val], @rhombus[fun], and @rhombus[struct] might report an error
if a transformer does not consume all available terms (and that's the
case in @Rhombus).
