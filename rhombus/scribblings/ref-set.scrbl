#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Sets}

Immutable sets can be constructed using the syntax
@rhombus({#,(@rhombus(val_expr, ~var)), ...}),
which creates a set containing the values of the @rhombus(val_expr, ~var)s.
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
  "readable set (immutable or mutable)"
  Set
  st.length()
  st.get(v)
  st.to_list(try_sort, ...)
  st.copy()
  st.snapshot()
)

@dispatch_table(
  "set (immutable only)"
  Set
  st.append(set2, ...)
  st.union(set2, ...)
  st.intersect(set2, ...)
  st.remove(v)
)

@dispatch_table(
  "mutable set"
  MutableSet
  st.set(v, in)
  st.delete(v)
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
  fun Set(val :: Any, ...) :: Set

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
 while @braces does not, since @rhombus({}) produces an empty map
 instead.

@examples(
  def s = Set{"x", 1, "y", 2}
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
 Static information associated by @rhombus(Set) is propagated to @rhombus(set_bind).
 If @rhombus(rest_bind) followed by @dots is
 supplied, the rest of the set excluding the given @rhombus(expr)s
 must have individual values that match @rhombus(rest_bind), and identifiers in
 @rhombus(rest_bind) are bound
 as repetitions. Values matching @rhombus(rest_bind)
 are extracted eagerly and preserved in an internal list to implement
 the repetition.

 The @rhombus(Set, ~bind) binding forms match only immutable sets, while
 @rhombus(ReadableSet, ~bind) forms match both immutable and mutable sets.
 For @rhombus(ReadableSet, ~bind), the @rhombus(& set_bind) will match
 a snapshot (in the sense of @rhombus(Set.snapshot)) of the rest of the set.

@examples(
  def Set{"x", "y"} = {"x", "y"}
  ~error:
    def Set{"x", "y"} = {"x"}
  def Set{"a"} = {"a", "b"}
  def Set{"a", & rst} = {"a", "b", "c"}
  rst
  def Set{"a", val, ...} = {"a", "b", "c"}
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
  fun MutableSet(val :: Any, ...) :: MutableSet
){

 Similar to @rhombus(Set) as a constructor, but creates a mutable set
 that can be updated using @rhombus(:=).

 Note that @dots_expr and @rhombus(&) are not supported for construction
 mutable sets, only immutable sets.

@examples(
  def m = MutableSet{"x", 1, "y", 2}
  m
  m["x"]
  m["x"] := #false
  m
  m["x"] := #true
  m
)

}

@doc(
  def Set.empty :: Set = Set{}
  bind.macro 'Set.empty'
  def ReadableSet.empty :: ReadableSet = Set{}
  bind.macro 'ReadableSet.empty'
){

 An empty set. The @rhombus(Set.empty, ~bind) binding form differs from
 from @rhombus(Set{}), because @rhombus(Set.empty, ~bind) matches only an
 empty immutable set, while @rhombus(Set{}) matches any immutable set.

 The @rhombus(ReadableSet.empty, ~bind) binding form matches an empty set
 whether it is mutable or immutable.

 Corresponding to the binding forms, @rhombus(Set.empty) and
 @rhombus(ReadableSet.empty) are bound to @rhombus(Set{}) with
 approapriate static information.

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
  fun Set.length(st :: ReadableSet) :: Int
){

 Returns the number of values in @rhombus(st).

@examples(
  Set.length({"a", "b"})
  Set.length(Set())
  {"a", "b"}.length()
)

}


@doc(
  fun Set.get(st :: ReadableSet, val :: Any) :: Boolean
){

 Equivalent to @rhombus(st[val]) (with the default implicit
 @rhombus(#%index) form). Returns @rhombus(#true) if @rhombus(val) is
 in @rhombus(st), @rhombus(#false) otherwise.

@examples(
  {"a", "b"}.get("a")
  {"a", "b"}["a"]
)

}


@doc(
  fun Set.append(st :: Set, ...) :: Set
  fun Set.union(st :: Set, ...) :: Set
  fun Set.intersect(st :: Set, ...) :: Set
){

 Returns the union of the @rhombus(st)s in the case of
 @rhombus(Set.append) or @rhombus(Set.union), or the intersection of the
 sets in the case of @rhombus(Set.intersect).

@examples(
  {1, 2, 3}.append({2, 3}, {3, 4})
  {1, 2, 3}.union({2, 3}, {3, 4})
  {1, 2, 3}.intersect({2, 3}, {3, 4})
  )

}

@doc(
  fun Set.remove(st :: Set, val :: Any) :: Set
){

 Returns a set like @rhombus(st) but without @rhombus(val), if it is
 present.

@examples(
  {1, 2, 3}.remove(2)
  {1, 2, 3}.remove(4)
)

}


@doc(
  fun MutableSet.set(st :: MutableSet,
                     val :: Any, in :: Any)
    :: Void
){

 Equivalent to @rhombus(st[val] := in) (with the default implicit
 @rhombus(#%index) form). Changes @rhombus(st) to remove
 @rhombus(val) if @rhombus(in) is @rhombus(#false), otherwise add
 @rhombus(val).

@examples(
  def s = MutableSet{1, 2, 3}
  s.set(1, #false)
  s
  s[1] := #true
  s
)

}


@doc(
  fun MutableSet.delete(st :: MutableSet, val :: Any)
    :: Void
){

 Changes @rhombus(st) to remove @rhombus(val), if it is present.

@examples(
  def s = MutableSet{1, 2, 3}
  s.delete(2)
  s
)

}


@doc(
  fun Set.to_list(st :: ReadableSet,
                  try_sort :: Any = #false)
    :: List
){

 Returns the elements of @rhombus(st) in a list. If @rhombus(try_sort)
 is true, then the elements are sorted to the degree that a built-in
 comparison can sort them. Note that sets do @emph{not} implement
 @rhombus(Listable, ~class), because the order of elements is
 unspecified.

@examples(
  {1, 2, 3}.to_list(#true)
)

}


@doc(
  fun Set.copy(st :: ReadableSet) :: MutableSet
){

 Creates a mutable set whose initial content matches @rhombus(st).

}


@doc(
  fun Set.snapshot(st :: ReadableSet) :: Set
){

 Returns an immutable set whose content matches @rhombus(st). If
 @rhombus(st) is immutable, then it is the result.

}
