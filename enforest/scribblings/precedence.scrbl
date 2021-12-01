#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Operator Precedence and Associativity}

Instead of attaching a numeric precedence to each operator, as in Honu,
each operator declares its precedence relative to other operators. That
is, an operator can declare that its precedence is stronger than certain
other operators, weaker than certain other operators, the same as
certain other operators (implicitly including the operator itself), and
the same as certain other operators when in a specific order (e.g.,
@rhombus[*] is not allowed to the right of @rhombus[/]). An infix
operator's associativity is relevant only for operators at the same
precedence (including the operator itself), as either left-associative,
right-associative, or non-associative.

Our example @rhombus[<>] definition did not specify any precedence
relationships, so it cannot be used next to @rhombus[*]:

@(rhombusblock:
    1 <> 2 * 3 // not allowed
  )

In this example, enforestation would report that @rhombus[<>] and
@rhombus[*] are unrelated, so parentheses are needed somewhere. @Rhombus
supports precedence declarations through a @rhombus[~weaker_than] keyword:

@(rhombusblock:
    operator (x <> y):
      ~weaker_than': * / + -
      Posn(x, y)

    1 <> 2 * 3 // same as Posn(1, 6)
)

Unlike a system based on numeric precedence levels, precedence in the
Rhombus expander is not a complete or partial order. It's not even
transitive. Operators @math{A} and @math{B} have a precedence
relationship only if either @math{A} indicates a relationship to
@math{B} or @math{B} indicates a relationship to @math{A}. This approach
turns out to work fine with a Pratt-style parser (as used in Honu),
which only ever needs to work with precedence when it has two specific
operators to compare. A potential advantage of non-transitive precedence
is avoiding an order among operands that make no sense next to each
other. An operator can declare a default precedence relationship to
other operators, but it must declare the default explicitly.

If two operators both claim a precedence relationship to each other, the
relationship must be consistent; for example, @math{A} cannot claim to
have stronger precedence than @math{B} if @math{B} claims stronger
precedence than @math{A}. Also, if one or both operators claim a same
precedence strength to the other, the operators must have the same
associativity. The consistency of precedence claims between @math{A} and
@math{B} is checked only at the point where @math{A} and @math{B} are
compared by the enforesting expander, and inconsistent claims trigger a
syntax error at that point.

Procedurally, precedence is relevant when enforestation encounters an
infix operator on the right-hand side of some other operator. Precedence
determines whether the terms in between the two operators form an
argument to the earlier operator (on the way to producing an argument
for the later operator) or whether those terms form an argument to the
newly encountered operator (on the way to producing an argument for the
earlier operator). Note that when a prefix operator is involved in a
precedence comparison, the other operator is always a later infix
operator.
