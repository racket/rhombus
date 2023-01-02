#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "multiple-values"){Multiple Values}

The @rhombus(values) form returns multiple values:

@demo(
  values(1, "apple")
)

When an expression in a module body returns multiple values, each one is
printed, the same as in @litchar{#lang racket}.

When the @rhombus(def) binding form is followed by parentheses with @math{N}
groups, then the right-hand side should produce @math{N} values, and each
value is matched against the corresponding group.

@demo(
  ~defn:
    def (n, s) = values(1, "apple")
  ~repl:
    n
    s
)

A definition binding with with @rhombus(val) or @rhombus(def) can also
use @rhombus(values) in the outermost pattern, and that’s the same as
not writing @rhombus(values), but it makes the receiver and sender side
look more the same:

@demo(
  ~defn:
    def values(n, s) = values(1, "apple")
  ~repl:
    n
    s
)

As in Racket, multiple values do not create a tuple value. They must be
specifically received as values. The @rhombus(values) binding pattern
works only with definition forms that recognize it, and not, for
example, as a function argument.
