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

To check for membership in a set, use @brackets after a set
expression with an expression for a value, and the result is a boolean
indicating whether the value is in the set. Mutable sets can be updated
with a combination of @brackets and the @rhombus(:=) operator, where
a @rhombus(#false) result on the right-hand side of @rhombus(:=) removes an
element from a set, and any other right-hand side result causes the value
to be included in the set. These uses of @brackets are implemented by
@rhombus(#%ref). A set can be used as @tech{sequence}, in which case
it supplies its elements in an unspecified order.

@dispatch_table(
  "set (immutable or mutable)"
  @rhombus(Set)
  [set.length(), Set.length(set)]
  [set.copy(), Set.copy(set)]
  [set.snapshot(), Set.snapshot(set)]
)

@doc(
  annot.macro 'Set'
  annot.macro 'Set.of($annot)'
  annot.macro 'SetView'
  annot.macro 'MutableSet'
){

 Matches any set in the form without @rhombus(of). The @rhombus(of)
 variant matches a set whose values satisfy @rhombus(annot).

 @rhombus(SetView, ~annot) matches both mutable and immutable maps,
 while @rhombus(MutableSet, ~annot) matches mutable maps (created with,
 for example, the @rhombus(MutableSet) constructor).

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
    set_bind: def bind
    rest_bind:  def bind
  bind.macro 'Set{$expr, ...}'
  bind.macro 'Set{$expr, ..., $rest}'
  bind.macro 'SetView{$expr, ...}'
  bind.macro 'SetView{$expr, ..., $rest}'
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
 as repetitions.

 The @rhombus(Set, ~bind) binding forms match only immutable sets, while
 @rhombus(SetView, ~bind) forms match both immutable and mutable sets.

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
  bind.macro 'SetView.empty'
  expr.macro 'SetView.empty'
){

 An empty set. The @rhombus(Set.empty, ~bind) binding form differs from
 from @rhombus(Set{}), because @rhombus(Set.empty, ~bind) matches only an
 empty immutable set, while @rhombus(Set{}) matches any immutable set.

 The @rhombus(SetView.empty, ~bind) binding form matches an empty set
 whether it is mutable or immutable. The @rhombus(SetView.empty)
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
  | SetView.empty: "empty set for now"
  | _: #false
)

}


@doc(
  fun Set.length(set :: Set) :: Int
){

 Returns the number of values in @rhombus(set).

@examples(
  Set.length({"a", "b"})
  Set.length(Set())
  {"a", "b"}.length()
)

}


@doc(
  fun Set.copy(set :: SetView) :: MutableSet
){

 Creates a mutable set whose initial content matches @rhombus(set).

}


@doc(
  fun Set.snapshot(set :: SetView) :: Set
){

 Returns an immutable set whose content matches @rhombus(set). If
 @rhombus(set) is immutable, then it is the result.

}
