#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Sets}

Immutable sets can be constructed using the syntax
@rhombus[{$$(@rhombus[val_expr, ~var]), ...}],
which creates a set containing the values of the @rhombus[value_expr, ~var]s.

To check for membership in a set, use square brackets after a map
expression with an expression for a value, and the result is a boolean
indicating whether the value is in the set. Mutable sets can be updated
with a combination of square brackets and the @rhombus[:=] operator, where
a @rhombus[#false] result on the right-hand side of @rhombus[:=] removes an
element from a set, and any other right-hand side result causes the value
to be inluded in the set.

@doc[
  fun Set(value:: Any, ...) :: Map
]{

 Constructs an immutable set containing given values, equivalent to
 using @rhombus[{value, ...}].

@examples[
  val s: Set("x", 1, "y", 2),
  s,
  s["x"],
  s[1],
  s[42]
]

}

@doc[
  annotation.macro 'Set,
  annotation.macro '(Set.of($annotation)),
]{

 Matches any set in the form without @rhombus[of]. The @rhombus[of]
 variant matches a set whose values satisfy @rhombus[annotation].

}


@doc[
  fun make_set(value:: Any, ...) :: Map
]{

 Similar to @rhombus[Set] as a constructor, but creates a mutable set
 that can be updated using @rhombus[:=].

@examples[
  val m: make_set("x", 1, "y", 2),
  m,
  m["x"],
  m["x"] := #false,
  m,
  m["x"] := #true,
  m
]

}
