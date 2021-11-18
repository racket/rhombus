#lang scribble/rhombus/manual
@(import: "util.rhm": no_prefix)

@title[~tag: "multiple-values"]{Multiple values}

The @rhombus[values] form returns multiple values:

@(rhombusblock:
    values(1, "apple") // prints 1 and "apple"
  )

When an expression in a module body returns multiple values, each one is
printed, the same as in @litchar{#lang racket}.

When the @rhombus[val] binding form is followed by parentheses with @math{N}
groups, then the right-hand side should produce @math{N} values, and each
value is matched against the corresponding group.

@(rhombusblock:
    val (n, s): values(1, "apple")

    n  // prints 1
    s  // prints "apple"
  )

A definition binding with with @rhombus[val] or @rhombus[def] can also
use @rhombus[values] in the outermost pattern, and that’s the same as
not writing @rhombus[values], but makes the receiver and sender side
look more the same:

@(rhombusblock:
    def values(n, s): values(1, "apple")

    n  // prints 1
    s  // prints "apple"
  )

As in Racket, multiple values are not a tuple value. They must be
specifically received as values. The @rhombus[values] binding pattern
works only with definition forms that recognize it, and not, for
example, as a function argument.
