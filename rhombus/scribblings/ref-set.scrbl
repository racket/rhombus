#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Sets}

Immutable sets can be constructed using the syntax
@rhombus({$$(@rhombus(val_expr, ~var)), ...}),
which creates a set containing the values of the @rhombus(value_expr, ~var)s.
More precisely, a use of curly braces with no preceding expression is
parsed as an implicit use of the @rhombus(#{#%set}) form.

To check for membership in a set, use square brackets after a map
expression with an expression for a value, and the result is a boolean
indicating whether the value is in the set. Mutable sets can be updated
with a combination of square brackets and the @rhombus(:=) operator, where
a @rhombus(#false) result on the right-hand side of @rhombus(:=) removes an
element from a set, and any other right-hand side result causes the value
to be included in the set. These uses of square brackets are implemented by
@rhombus(#{#%ref}).

The @rhombus(.) operator can be used on a list expression with
@rhombus(count) to call @rhombus(Set.count).

@doc(
  expr.macro 'Set{$value_expr, ...}',
  fun Set(value:: Any, ...) :: Map
){

 Constructs an immutable set containing given values, equivalent to
 using @rhombus({value_expr, ...}).

@examples(
  val s: Set{"x", 1, "y", 2},
  s,
  s["x"],
  s[1],
  s[42],
  Set("x", 1, "y", 2)
)

}

@doc(
  annotation.macro 'Set',
  annotation.macro 'Set.of($annotation)',
){

 Matches any set in the form without @rhombus(of). The @rhombus(of)
 variant matches a set whose values satisfy @rhombus(annotation).

}


@doc(
  expr.macro 'MutableSet{$value_expr, ...}',
  fun MutableSet(value:: Any, ...) :: Set
){

 Similar to @rhombus(Set) as a constructor, but creates a mutable set
 that can be updated using @rhombus(:=).

@examples(
  val m: MutableSet{"x", 1, "y", 2},
  m,
  m["x"],
  m["x"] := #false,
  m,
  m["x"] := #true,
  m
)

}

@doc(
  bind.macro 'Set.empty',
  expr.macro 'Set.empty'
){

 An empty set, where the @rhombus(Set.empty, ~bind) binding matches
 only an empty set (mutable or immutable).

@examples(
  Set.empty,
  match Set()
  | Set.empty: "empty set"
  | _: #false
)

}


@doc(
  fun Set.count(set :: Set) :: Integer,
){

 Returns the number of values in @rhombus(set).

@examples(
  Set.count({"a", "b"}),
  Set.count(Set()),
  {"a", "b"}.count
  )

}
