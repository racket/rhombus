#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title{Sets}

Immutable sets can be constructed using the syntax
@rhombus({#,(@rhombus(val_expr, ~var)), ...}),
which creates a set containing the values of the @rhombus(value_expr, ~var)s.
More precisely, a use of curly braces with no preceding expression is
parsed as an implicit use of the @rhombus(#%braces) form.

A set is @tech{indexable}, and @brackets after a set
expression with an expression for a value checks for membership: the result is a boolean
indicating whether the value is in the set. Mutable sets can be updated
with a combination of @brackets and the @rhombus(:=) operator, where
a @rhombus(#false) result on the right-hand side of @rhombus(:=) removes an
element from a set, and any other right-hand side result causes the value
to be included in the set. (Use @rhombus(++) to functionally add to an
immutable set.) These uses of @brackets are implemented by
@rhombus(#%index). A set can be used as @tech{sequence}, in which case
it supplies its elements in an unspecified order.

@dispatch_table(
  "set (immutable or mutable)"
  @rhombus(Set)
  [set.length(), Set.length(set)]
  [set.append(set2, ...), Set.length(set, set2, ...)]
  [set.union(set2, ...), Set.union(set, set2, ...)]
  [set.intersect(set2, ...), Set.intersect(set, set2, ...)]
  [set.remove(v), Set.remove(set, v)]
  [set.to_list(), Set.to_list(set)]
  [set.copy(), Set.copy(set)]
  [set.snapshot(), Set.snapshot(set)]
)

@doc(
  annot.macro 'Set'
  annot.macro 'Set.of($annot)'
  annot.macro 'ReadableSet'
  annot.macro 'MutableSet'
){

 Matches any set in the form without @rhombus(of). The @rhombus(of)
 variant matches a set whose values satisfy @rhombus(annot).

 @rhombus(ReadableSet, ~annot) matches both mutable and immutable maps,
 while @rhombus(MutableSet, ~annot) matches mutable maps (created with,
 for example, the @rhombus(MutableSet) constructor).

 Static information associated by @rhombus(Set, ~annot), etc., makes
 an expression acceptable to @rhombus(for) in static mode.

}

@doc(
  ~nonterminal:
    set_expr: block expr
  expr.macro 'Set{$expr_or_splice, ...}'
  repet.macro 'Set{$repet_or_splice, ...}'
  fun Set(value:: Any, ...) :: Set

  grammar expr_or_splice:
    $expr
    $repet #,(@litchar{,}) ellipses
    & $set_expr

  grammar ellipses:
    $ellipsis
    $ellipses #,(@litchar{,}) $ellipsis

  grammar ellipsis:
    #,(dots_expr)

){

 Constructs an immutable set containing given values, equivalent to
 using @rhombus({expr_or_splice, ...}) to form a set (see
 @rhombus(#%braces)).

 Note that @rhombus(Set{}) and @rhombus(Set()) produce an empty set
 while @rhombus({}) does not, since @rhombus({}) produces an empty map
 instead.

@examples(
  def s: Set{"x", 1, "y", 2}
  s
  s["x"]
  s[1]
  s[42]
  Set("x", 1, "y", 2)
)

}

@doc(
  ~nonterminal:
    set_bind: def bind ~defn
    rest_bind:  def bind ~defn
  bind.macro 'Set{$expr, ...}'
  bind.macro 'Set{$expr, ..., $rest}'
  bind.macro 'ReadableSet{$expr, ...}'
  bind.macro 'ReadableSet{$expr, ..., $rest}'
  grammar rest:
    & $set_bind
    $rest_bind #,(@litchar{,}) $ellipsis
  grammar ellipsis:
    #,(dots)
){

 Matches a set containing at least the values computed by the @rhombus(expr)s.
 The matched set may have additional values.
 If @rhombus(& set_bind) is supplied, the rest of the set excluding
 the values of the given @rhombus(expr)s must match the @rhombus(set_bind).
 If @rhombus(rest_bind) followed by @dots is
 supplied, the rest of the set excluding the given @rhombus(expr)s
 must have individual values that match @rhombus(rest_bind), and identifiers in
 @rhombus(rest_bind) are bound
 as repetitions. Values matching @rhombus(rest_bind)
 are extracted eagerly and preserved in an internal list to implement
 the repetition.

 The @rhombus(Set, ~bind) binding forms match only immutable sets, while
 @rhombus(ReadableSet, ~bind) forms match both immutable and mutable sets.

@examples(
  def Set{"x", "y"}: {"x", "y"}
  ~error:
    def Set{"x", "y"}: {"x"}
  def Set{"a"}: {"a", "b"}
  def Set{"a", & rst}: {"a", "b", "c"}
  rst
  def Set{"a", val, ...}: {"a", "b", "c"}
  [val, ...]
)

}


@doc(
  reducer.macro 'Set'
){

 A @tech{reducer} used with @rhombus(for), accumulates values into a
 set.

}


@doc(
  ~nonterminal:
    val_expr: block expr
  expr.macro 'MutableSet{$val_expr, ...}'
  fun MutableSet(value:: Any, ...) :: MutableSet
){

 Similar to @rhombus(Set) as a constructor, but creates a mutable set
 that can be updated using @rhombus(:=).

 Note that @dots_expr and @rhombus(&) are not supported for construction
 mutable sets, only immutable sets.

@examples(
  def m: MutableSet{"x", 1, "y", 2}
  m
  m["x"]
  m["x"] := #false
  m
  m["x"] := #true
  m
)

}

@doc(
  bind.macro 'Set.empty'
  expr.macro 'Set.empty'
  bind.macro 'ReadableSet.empty'
  expr.macro 'ReadableSet.empty'
){

 An empty set. The @rhombus(Set.empty, ~bind) binding form differs from
 from @rhombus(Set{}), because @rhombus(Set.empty, ~bind) matches only an
 empty immutable set, while @rhombus(Set{}) matches any immutable set.

 The @rhombus(ReadableSet.empty, ~bind) binding form matches an empty set
 whether it is mutable or immutable. The @rhombus(ReadableSet.empty)
 expression form is equivalent to @rhombus(Set.empty).

 An empty set, where the @rhombus(Set.empty, ~bind) binding matches
 only an empty set.

@examples(
  Set.empty
  match Set()
  | Set.empty: "empty set"
  | _: #false
  match MutableSet()
  | Set.empty: "empty immutable set"
  | _: #false
  match MutableSet()
  | ReadableSet.empty: "empty set for now"
  | _: #false
)

}


@doc(
  fun Set.length(set :: ReadableSet) :: Int
){

 Returns the number of values in @rhombus(set).

@examples(
  Set.length({"a", "b"})
  Set.length(Set())
  {"a", "b"}.length()
)

}

@doc(
  fun Set.append(set :: ReadableSet, ...) :: Set
  fun Set.union(set :: ReadableSet, ...) :: Set
  fun Set.intersect(set :: ReadableSet, ...) :: Set
){

 Returns the union of the @rhombus(set)s in the case of
 @rhombus(Set.append) or @rhombus(Set.union), or the intersection of the
 sets in the case of @rhombus(Set.intersect).

@examples(
  {1, 2, 3}.append({2, 3}, {3, 4})
  {1, 2, 3}.union({2, 3}, {3, 4})
  {1, 2, 3}.intersect({2, 3}, {3, 4})
  )

}

@doc(
  fun Set.remove(set :: Set, v) :: Set
){

 Returns a set like @rhombus(v) from @rhombus(set), if it is present.

@examples(
  {1, 2, 3}.remove(2)
  {1, 2, 3}.remove(4)
)

}

@doc(
  fun Set.to_list(set :: Set, try_order = #false) :: List
){

 Returns the elements of @rhombus(set) in a list. If @rhombus(try_order)
 is true, then the elements are sorted to the degree that a built-in
 comparison can sort them.

@examples(
  {1, 2, 3}.to_list(#true)
)

}


@doc(
  fun Set.copy(set :: ReadableSet) :: MutableSet
){

 Creates a mutable set whose initial content matches @rhombus(set).

}


@doc(
  fun Set.snapshot(set :: ReadableSet) :: Set
){

 Returns an immutable set whose content matches @rhombus(set). If
 @rhombus(set) is immutable, then it is the result.

}
