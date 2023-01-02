#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title{Sets}

Immutable sets can be constructed using the syntax
@rhombus({$$(@rhombus(val_expr, ~var)), ...}),
which creates a set containing the values of the @rhombus(value_expr, ~var)s.
More precisely, a use of curly braces with no preceding expression is
parsed as an implicit use of the @rhombus(#{#%braces}) form.

To check for membership in a set, use square brackets after a map
expression with an expression for a value, and the result is a boolean
indicating whether the value is in the set. Mutable sets can be updated
with a combination of square brackets and the @rhombus(:=) operator, where
a @rhombus(#false) result on the right-hand side of @rhombus(:=) removes an
element from a set, and any other right-hand side result causes the value
to be included in the set. These uses of square brackets are implemented by
@rhombus(#{#%ref}).

@dispatch_table(
  "set",
  @rhombus(Set),
  [set.length(), Set.length(set)]
)

@doc(
  expr.macro 'Set{$expr_or_splice, ...}',
  repet.macro 'Set{$repet_or_splice, ...}',
  fun Set(value:: Any, ...) :: Set,

  grammar expr_or_splice:
    $expr
    $repetition $$(@litchar{,}) $$(dots_expr)
    & $set_expr    
){

 Constructs an immutable set containing given values, equivalent to
 using @rhombus({expr_or_splice, ...}) to form a set (see
 @rhombus(#{#%braces})).

 Note that @rhombus(Set{}) and @rhombus(Set()) produce an empty set
 while @rhombus({}) does not, since @rhombus({}) produces an empty map
 instead.

@examples(
  def s: Set{"x", 1, "y", 2},
  s,
  s["x"],
  s[1],
  s[42],
  Set("x", 1, "y", 2)
)

}

@doc(
  bind.macro 'Set{$expr, ...}',
  bind.macro 'Set{$expr, ..., $rest}',
  grammar rest:
    & $set_binding
    $rest_binding $$(@litchar{,}) $$(dots)
){

 Matches a set containing at least the values computed by the @rhombus(expr)s.
 The matched set may have additional values.
 If @rhombus(& set_binding) is supplied, the rest of the set excluding
 the values of the given @rhombus(expr)s must match the @rhombus(set_binding).
 If @rhombus(rest_binding) followed by @dots is
 supplied, the rest of the set excluding the given @rhombus(expr)s
 must have individual values that match @rhombus(rest_binding), and identifiers in
 @rhombus(rest_binding) are bound
 as repetitions.

@examples(
  def Set{"x", "y"}: {"x", "y"},
  ~error:
    def Set{"x", "y"}: {"x"},
  def Set{"a"}: {"a", "b"},
  def Set{"a", & rst}: {"a", "b", "c"},
  rst,
  def Set{"a", val, ...}: {"a", "b", "c"},
  [val, ...]
)

}


@doc(
  annot.macro 'Set',
  annot.macro 'Set.of($annotation)',
){

 Matches any set in the form without @rhombus(of). The @rhombus(of)
 variant matches a set whose values satisfy @rhombus(annotation).

}


@doc(
  reducer.macro 'Set'
){

 A @tech{reducer} used with @rhombus(for), accumulates values into a
 set.

}


@doc(
  expr.macro 'MutableSet{$value_expr, ...}',
  fun MutableSet(value:: Any, ...) :: Set
){

 Similar to @rhombus(Set) as a constructor, but creates a mutable set
 that can be updated using @rhombus(:=).

 Note that @dots_expr and @rhombus(&) are not supported for construction
 mutable sets, only immutable sets.

@examples(
  def m: MutableSet{"x", 1, "y", 2},
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
  fun Set.length(set :: Set) :: Integer,
){

 Returns the number of values in @rhombus(set).

@examples(
  Set.length({"a", "b"}),
  Set.length(Set()),
  {"a", "b"}.length()
  )

}
