#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Sets}

A @deftech{set}@intro_note("set", "sets") is an unordered collection of elements, where a value
can appear as an element only once. A set's length is the number of
elements that it contains.

Immutable sets can be constructed using the syntax
@rhombus({#,(@rhombus(val_expr, ~var)), ...}),
which creates a set containing the values of the @rhombus(val_expr, ~var)s.
More precisely, a use of curly braces with no preceding expression is
parsed as an implicit use of the @rhombus(#%braces) form.

A set supports @tech{membership tests} with the @rhombus(in) operator.
Use @rhombus(++) or @rhombus(Set.add) to functionally add to an
immutable set. A set can be used as @tech{sequence}, in which case
it supplies its elements in an unspecified order.

Like @tech{maps}, aset is normally immutable, but a mutable set can be
created by operations such as @rhombus(Set.copy), mutable sets may hold
their elements weakly, and different sets can use different
@tech(~doc: meta_doc){map configurations} that determine the functions
used for comparing and hashing set elements.

Two immutable sets tables are equal by @rhombus(==) when they have the
same same map configuration and the same elements (where ``same'' is
determined by the map configuration). Two sets are equal by
@rhombus(is_now) when they have the same mutability, both hold their
elements strongly or both weakly, and have the same elements.

@doc(
  annot.macro 'Set'
  annot.macro 'Set.of($annot)'
  annot.macro 'Set.later_of($annot)'
  annot.macro 'ReadableSet':
    ~method_fallback: Set
  annot.macro 'MutableSet':
    ~method_fallback: Set
  annot.macro 'WeakMutableSet':
    ~method_fallback: Set
  annot.macro 'MutableSet.now_of($annot)'
  annot.macro 'MutableSet.later_of($annot)'
  annot.macro 'Set.by($key_comp)'
  annot.macro 'Set.by($key_comp).of($annot)'
  annot.macro 'MutableSet.by($key_comp)'
  annot.macro 'WeakMutableSet.by($key_comp)'
){

 The @rhombus(Set, ~annot) annotation matches any set.
 @rhombus(ReadableSet, ~annot) matches both mutable and immutable sets,
 while @rhombus(MutableSet, ~annot) matches mutable sets (created with,
 for example, the @rhombus(MutableSet) constructor).
 @rhombus(WeakMutableSet, ~annot) matches weak mutable sets (created with, for
 example, the @rhombus(WeakMutableSet) constructor).

 The @rhombus(of) and @rhombus(now_of) annotation variants match a set
 whose elements satisfy @rhombus(annot), where satisfaction of the
 annotation is confirmed by immediately checking all elements. No future
 obligation is attached to a set satisfying the annotation, so in the
 case of @rhombus(MutableSet.now_of), no static information is associated
 with value access using @brackets.

 The @rhombus(later_of) annotation variants create a @tech(~doc: model_doc){converter
  annotation} given an annotations for elements; satisfaction of those
 annotations is confirmed only on demand, both for elements that are
 extracted from the set and for elements added or appended to the set.
 For @rhombus(Set.later_of, ~annot), the key and value annotations must be
 @tech(~doc: model_doc){predicate annotations}. Since an element annotation is checked on
 every access, its static information is associated with access using
 @brackets. If @rhombus(annot) is an
 @tech(~doc: model_doc){always-satisfied annotation}, then the @rhombus(later_of)
 variants produce @tech(~doc: model_doc){predicate annotations}, because
 no conversion is needed.

 Note that @rhombus(Any.like_element, ~annot) will not find any static
 information for elements from an expression with an
 @rhombus(MutableSet, ~annot) or @rhombus(MutableSet.now_of, ~annot)
 annotation, but an @rhombus(MutableSet.later_of, ~annot) annotation can
 imply static information for elements.

 The @rhombus(Set.by, ~annot) and @rhombus(MutableSet.by, ~annot)
 annotation variants match only sets that use the hash and equality
 functions specified by @rhombus(key_comp).

 Static information associated by @rhombus(Set, ~annot), etc., makes
 an expression acceptable to @rhombus(for) in static mode.

}

@doc(
  ~nonterminal:
    set_expr: block expr
  expr.macro 'Set{$expr_or_splice, ...}'
  repet.macro 'Set{$repet_or_splice, ...}'

  grammar expr_or_splice
  | $expr
  | $repet #,(@litchar{,}) ellipses
  | & $set_expr

  grammar ellipses
  | $ellipsis
  | $ellipses #,(@litchar{,}) $ellipsis

  grammar ellipsis
  | #,(dots_expr)

  fun Set(val :: Any, ...) :: Set.of(Any.like(val))
  expr.macro 'Set.by($key_comp){$expr_or_splice, ...}'
  expr.macro 'Set.by($key_comp)'
  repet.macro 'Set.by($key_comp){$repet_or_splice, ...}'
){

 Constructs an immutable set containing given values, equivalent to
 using @rhombus({expr_or_splice, ...}) to form a set (see
 @rhombus(#%braces)).

 Note that @rhombus(Set{}) and @rhombus(Set()) produce an empty set
 while @braces does not, since @rhombus({}) produces an empty map
 instead.

 The @rhombus(Set.by) variants create a set that uses the equality and
 hashing functions specified by @rhombus(key_comp).

@examples(
  def s = Set{"x", 1, "y", 2}
  s
  "x" in s
  1 in s
  42 in s
  Set("x", 1, "y", 2)
)

}

@doc(
  ~nonterminal:
    set_bind: def bind ~defn
    rest_bind: def bind ~defn
  bind.macro 'Set{$expr, ...}'
  bind.macro 'Set{$expr, ..., $rest}'
  bind.macro 'Set($expr, ...)'
  bind.macro 'Set($expr, ..., $rest)'
  bind.macro 'ReadableSet{$expr, ...}'
  bind.macro 'ReadableSet{$expr, ..., $rest}'
  bind.macro 'ReadableSet($expr, ...)'
  bind.macro 'ReadableSet($expr, ..., $rest)'
  bind.macro 'Set.by($key_comp){$expr, ...}'
  bind.macro 'Set.by($key_comp){$expr, ..., $rest}'
  bind.macro 'Set.by($key_comp)($expr, ...)'
  bind.macro 'Set.by($key_comp)($expr, ..., $rest)'
  grammar rest
  | #,(@rhombus(&, ~bind)) $set_bind
  | $rest_bind #,(@litchar{,}) $ellipsis
  grammar ellipsis
  | #,(dots)
){

 Matches a set containing at least the values computed by the @rhombus(expr)s.
 The matched set may have additional values.
 If @rhombus(#,(@rhombus(&, ~bind)) set_bind) is supplied, the rest of the set excluding
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
 For @rhombus(ReadableSet, ~bind), the @rhombus(#,(@rhombus(&, ~bind)) set_bind) will match
 a snapshot (in the sense of @rhombus(Set.snapshot)) of the rest of the set.
 The @rhombus(Set.by, ~bind) binding forms match only immutable sets
 constructed using @rhombus(key_comp).

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
  reducer.macro 'Set.by($key_comp)'
){

 A @tech(~doc: guide_doc){reducer} used with @rhombus(for), accumulates values into a
 set.

 The @rhombus(Set.by, ~reducer) reducer creates a set that uses the
 equality and hashing functions specified by @rhombus(key_comp).

}


@doc(
  ~nonterminal:
    val_expr: block expr
  expr.macro 'MutableSet{$val_expr, ...}'
  fun MutableSet(val :: Any, ...) :: MutableSet
  expr.macro 'MutableSet.by($key_comp){$val_expr, ...}'
  expr.macro 'MutableSet.by($key_comp)'
){

 Similar to @rhombus(Set) as a constructor, but creates a mutable set
 that can be updated using an @tech(~doc: meta_doc){assignment operator} like @rhombus(:=).

 Note that @dots_expr and @rhombus(&) are not supported for construction
 mutable sets, only immutable sets.

@examples(
  def m = MutableSet{"x", 1, "y", 2}
  m
  "x" in m
  m.remove("x")
  m
)

}


@doc(
  ~nonterminal:
    val_expr: block expr
  expr.macro 'WeakMutableSet{$val_expr, ...}'
  fun WeakMutableSet(val :: Any, ...) :: MutableSet
  expr.macro 'WeakMutableSet.by($key_comp){$val_expr, ...}'
  expr.macro 'WeakMutableSet.by($key_comp)'
){


 Like @rhombus(MutableSet), but creates a set where an element is
 removed from the set by a garbage collection when the element is
 reachable only by enumerating the set's elements.

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
 appropriate static information.

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
  method Set.length(st :: ReadableSet) :: Int
){

 Returns the number of values in @rhombus(st).

@examples(
  Set.length({"a", "b"})
  Set.length(Set{})
  {"a", "b"}.length()
  Set{}.length()
)

}


@doc(
  method Set.contains(st :: ReadableSet, val :: Any) :: Boolean
){

 Returns @rhombus(#true) if @rhombus(val) is in @rhombus(st),
 @rhombus(#false) otherwise. See also @rhombus(in).

@examples(
  {"a", "b"}.contains("a")
  "a" in {"a", "b"}
)

}


@doc(
  method (st :: Set).append(another_st :: Set, ...)
    :: Set.of(Any.like_element(st) || Any.like_element(another_st))
  method (st :: Set).union(another_st :: Set, ...)
    :: Set.of(Any.like_element(st) || Any.like_element(another_st))
){

 Functionally appends @rhombus(st) and @rhombus(another_st)s, like the @rhombus(++) operator
 (but without the special optimization for adding a single element).

 Even when @rhombus(another_st) uses a different @tech(~doc: meta_doc){map
  configuration} than @rhombus(st), the @tech(~doc: meta_doc){map configuration} of @rhombus(st)
 is preserved in the result set. Conceptually, in the binary case, each
 element from the right set is added to the left set.

@examples(
  {1, 2, 3}.append({2, 3}, {3, 4})
  {1, 2, 3}.union({2, 3}, {3, 4})
  {1, 2, 3} ++ {2, 3} ++ {3, 4}
  {1, 2, 3}.append(
    Set.by(is_same_number_or_object){2, 3},
    Set.by(is_now){3, 4},
  )
  {1, 2, 3}.union(
    Set.by(is_same_number_or_object){2, 3},
    Set.by(is_now){3, 4},
  )
  {1, 2, 3}
    ++ Set.by(is_same_number_or_object){2, 3}
    ++ Set.by(is_now){3, 4}
)

}

@doc(
  method (st :: Set).intersect(another_st :: Set, ...)
    :: Set.of(Any.like_element(st))
){

 Returns the intersection of @rhombus(st) and @rhombus(another_st)s.

 Even when some @rhombus(another_st) uses a different @tech(~doc: meta_doc){map
  configuration} than @rhombus(st), the @tech(~doc: meta_doc){map configuration} of @rhombus(st)
 is preserved in the result set.

@examples(
  {1, 2, 3}.intersect({2, 3}, {3, 4})
  {1, 2, 3}.intersect(
    Set.by(is_same_number_or_object){2, 3},
    Set.by(is_now){3, 4},
  )
)

}

@doc(
  method (st :: Set).subtract(another_st :: Set, ...)
    :: Set.of(Any.like_element(st))
){

 Returns the subtract from @rhombus(st) of @rhombus(another_st)s.

 Even when some @rhombus(another_st) uses a different @tech(~doc: meta_doc){map
  configuration} than @rhombus(st), the @tech(~doc: meta_doc){map configuration} of @rhombus(st)
 is preserved in the result set.

@examples(
  {1, 2, 3}.subtract({0, 2}, {3, 4, 5})
  {1, 2, 3}.subtract(
    Set.by(is_same_number_or_object){0, 2},
    Set.by(is_now){3, 4, 5},
  )
)

}

@doc(
  method (st :: Set).add(val :: Any)
    :: Set.of(Any.like_element(st) || Any.like(val))
){

 Returns a set like @rhombus(st), but with @rhombus(val) added if it is
 not already present, like @rhombus(st ++ {val}).

@examples(
  {1, 2, 3}.add(4)
  {1, 2, 3}.add(2)
)

}

@doc(
  method (st :: Set).remove(val :: Any)
    :: Set.of(Any.like_element(st))
){

 Returns a set like @rhombus(st), but without @rhombus(val) if it is
 present.

@examples(
  {1, 2, 3}.remove(2)
  {1, 2, 3}.remove(4)
)

}


@doc(
  method (st :: MutableSet).add(val :: Any)
    :: Void
){

 Changes @rhombus(st) to add @rhombus(val) to the set.

@examples(
  def s = MutableSet{1, 2, 3}
  s.add(1)
  s
)

}


@doc(
  method (st :: MutableSet).remove(val :: Any)
    :: Void
){

 Changes @rhombus(st) to remove @rhombus(val), if it is present.

@examples(
  def s = MutableSet{1, 2, 3}
  s.remove(2)
  s
)

}


@doc(
  method Set.to_list(st :: ReadableSet,
                     try_sort :: Any = #false)
    :: List.of(Any.like_element(st))
){

 Returns the elements of @rhombus(st) in a list. If @rhombus(try_sort)
 is true, then the elements are sorted to the degree that a built-in
 comparison can sort them, otherwise the order is unspecified. Note
 that sets do @emph{not} implement
 @rhombus(Listable, ~class), because the order of elements is
 unspecified.

@examples(
  {1, 2, 3}.to_list(#true)
  {"a", "b", "c"}.to_list(#true)
  :
    {"a", "b", "c"}.to_list() // order is unspecified
  ~error:
    [& {"a", "b", "c"}]
)

}


@doc(
  method Set.copy(st :: ReadableSet) :: MutableSet
){

 Creates a mutable set whose initial content matches @rhombus(st).

@examples(
  ~repl:
    def s = {"a", "b"}
    s.copy()
  ~repl:
    def s = MutableSet{"a", "b"}
    s.copy()
)

}


@doc(
  method Set.snapshot(st :: ReadableSet)
    :: List.of(Any.like_element(st))
){

 Returns an immutable set whose content matches @rhombus(st). If
 @rhombus(st) is immutable, then it is the result.

@examples(
  ~repl:
    def s = {"a", "b"}
    s.snapshot()
    s.snapshot() === s
  ~repl:
    def s = MutableSet{"a", "b"}
    s.snapshot()
)

}


@doc(
  method Set.to_sequence(st :: ReadableSet)
    :: Sequence.assume_of(Any.like_element(st))
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(st)'s elements in an unspecified order.

@examples(
  :
    for List (v in {1, 2, 3}): // optimizing
      v
  :
    for List (v in {1, 2, 3}.to_sequence()): // non-optimizing
      v
  :
    for List (v in {"a", "b", "c"}): // order is unspecified
      v
)

}
