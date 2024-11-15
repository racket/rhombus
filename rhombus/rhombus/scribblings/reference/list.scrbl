#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Lists}

@deftech{Lists} can be constructed using the syntax
@rhombus([#,(@rhombus(expr, ~var)), ...]), which creates list containing the values of the
@rhombus(expr, ~var)s as elements. More precisely, a use of square
brackets without a preceding expression implicitly uses the
@rhombus(#%brackets) form, which is normally bound to
construct a list.

A list is @tech{indexable} using @brackets to access a list
element by position---in @math{O(log N)} time---via
@rhombus(#%index). A list also works with the @rhombus(++) operator
to append lists. A list can be used as @tech{sequence}, in which case
it supplies its elements in order.

@doc(
  annot.macro 'List'
  annot.macro 'List.of($annot)'
){

 Matches any list in the form without @rhombus(of). The @rhombus(of)
 variant matches a list whose elements satisfy @rhombus(annot).

 Static information associated by @rhombus(List, ~annot) or
 @rhombus(List.of, ~annot) makes an expression acceptable as a sequence
 to @rhombus(for) in static mode.

}

@doc(
  ~nonterminal:
    listable_expr: block expr
  fun List(v :: Any, ...) :: List
  expr.macro '#%brackets [$expr_or_splice, ...]'
  repet.macro '#%brackets [$repet_or_splice, ...]'
  expr.macro 'List[$expr_or_splice, ...]'
  repet.macro 'List[$repet_or_splice, ...]'

  grammar expr_or_splice:
    $expr
    $repet #,(@litchar{,}) $ellipses
    & $listable_expr

  grammar ellipses:
    $ellipsis
    $ellipses #,(@litchar{,}) ellipsis

  grammar ellipsis:
    #,(dots_expr)

){

 Constructs a list of the given @rhombus(v)s values or results of the
 @rhombus(expr_or_splice)s. A @rhombus(&) or @dots_expr form
 can appear within @brackets to splice a @tech{repetition} or existing @tech{listable}
 value into the constructed list, the same as in a function call (see
 @rhombus(#%call)). List constructions can also serve as
 repetitions, where @rhombus(repet_or_splice) is like
 @rhombus(expr_or_splice), but with repetitions in place of expressions.

 @see_implicit(@rhombus(#%brackets), @brackets, "expression")

@examples(
  def lst = List(1, 2, 3)
  lst
  lst[0]
  lst ++ [4, 5]
  #%brackets [1, 2, 3]
)

}

@doc(
  ~nonterminal:
    list_bind: def bind ~defn
    list_repet_bind: def bind ~defn
    repet_bind: def bind ~defn
  bind.macro 'List($bind_or_splice, ...)'
  bind.macro '#%brackets [$bind_or_splice, ...]'
  bind.macro 'List[$bind_or_splice, ...]'
  grammar bind_or_splice:
    $bind
    $splice
  grammar splice:
    $repet_bind #,(@litchar{,}) $ellipsis
    #,(@rhombus(&, ~bind)) $list_bind
    #,(@rhombus(&, ~bind)) $list_repet_bind #,(@litchar{,}) $ellipsis
  grammar ellipsis:
    #,(dots)
){

 Matches a list with as many elements as @rhombus(bind)s, or if a
 @rhombus(splice) is included, at least as many elements as
 @rhombus(bind)s. Each @rhombus(splice) matches a sublist, and the
 matching sublist is deterministic if only one @rhombus(splice) is
 present with @rhombus(&) or @dots (but not both).

@examples(
  def List(1, x, y) = [1, 2, 3]
  y
  def [1, also_x, also_y] = [1, 2, 3]
  also_y
  def List(1, & xs) = [1, 2, 3]
  xs
  def List(1, x, ...) = [1, 2, 3]
  [x, ...]
  def List(1, x, ..., 3) = [1, 2, 3]
  [x, ...]
)

 If multiple @rhombus(splice)s are present, matching is greedy: the
 first @rhombus(splice) matches as many elements as possible to achieve a
 successful match, and so on for subsequent matches with the remaining
 elements.

@examples(
  def List(x, ..., z, ...) = [1, 2, "c", 5]
  [[x, ...], [z, ...]]
  def List(x, ..., #'stop, z, ...) = [1, 2, "c", #'stop, 5]
  [[x, ...], [z, ...]]
  def List(x :: Int, ..., y, z, ...) = [1, 2, "c", "d", 5]
  [[x, ...], y, [z, ...]]
)

 For a
 @rhombus(#,(@rhombus(&, ~bind)) list_repet_bind #,(@litchar{,})  ellipsis)
 splice repetition, @rhombus(list_repet_bind) is matched greedily to a
 non-empty sequence for each repetition of the splice.

@examples(
  def List(& [x, ...], ...) = [1, 2, 3, 4]
  [[x, ...], ...]
  def List(& [x :: Int, ..., y], ..., z, ...) = [1, 2, "c", 4, "e", 6]
  [[[x, ...], ...], [y, ...], [z, ...]]
)

 When @rhombus(splice) does not impose a predicate or conversion on a
 matching value (e.g., @rhombus(repet_bind) or @rhombus(list_bind) is an
 identifier), then the corresponding elements of a matching value are not
 traversed, which means that matching takes only the time needed by
 @rhombus(List.sublist) (practically constant time) for a single such
 splice. Using this repetition in a new list similarly avoids traversing
 the elements.

 A splice of the form @rhombus(#,(@rhombus(&, ~bind)) list_bind) may
 have a statically known length. In particular, static information with
 the key @rhombus(statinfo_meta.list_bounds_key) is associated with the
 overall pattern, so if @rhombus(list_bind) is based on a list pattern,
 it may report its bounds via that key. The list matcher will not attempt
 to matches that are inconsistent with known bounds, and a
 multi-@rhombus(splice) pattern using
 @rhombus(#,(@rhombus(&, ~bind)) list_bind) splices is potentially
 deterministic and efficient if bounds can be determined statically. The
 same is true for @rhombus(list_repet_bind) in a splice repetition, but
 the list matcher can only recompute expected bounds for a list size, not
 an expected modulus for a list size.

 When multiple @rhombus(splice)s are present, the search for a match to
 each @rhombus(repet_bind #,(@litchar{,}) ellipsis) splice first builds
 up a candidate sequence as long as possible by stopping when an element
 doesn't satisfy @rhombus(repet_bind), and then the matcher backtracks to
 shorter sequences as needed to find an overall match. The search for a
 match to @rhombus(#,(@rhombus(&, ~bind)) list_bind) tries first matching
 the longest plausible sequence to @rhombus(list_bind), then backtracks
 as needed by trying smaller sequences. Matching a splice repetition
 combines these strategies: search uses the longer plausible candidiate
 for a given repetition, and it builds up repetitions until no
 (non-empty) match is found for a repetition.

 In the case of a @rhombus(#,(@rhombus(&, ~bind)) list_bind) splice or
 @rhombus(#,(@rhombus(&, ~bind)) list_repet_bind #,(@litchar{,}) ellipsis)
 splice repetition, static information associated by @rhombus(List) is
 propagated to @rhombus(list_bind) or @rhombus(list_repet_bind).

 @see_implicit(@rhombus(#%brackets, ~bind), @brackets, "binding")
}

@doc(
  annot.macro 'NonemptyList'
  annot.macro 'NonemptyList.of($annot)'
){

 Like @rhombus(List, ~annot) as an annotation, but matches only non-empty
 lists.

@examples(
  [1] :: NonemptyList
  ~error:
    [] :: NonemptyList
)

}

@doc(
  reducer.macro 'List'
){

 A @tech(~doc: guide_doc){reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result list.

}

@doc(
  method (lst :: List).insert(n :: NonnegInt, elem :: Any) :: List
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added before
 the @rhombus(n)th element or at the end if @rhombus(n) is the length of
 @rhombus(lst). The @rhombus(n) index must be no more than the length of
 @rhombus(lst). Insertion takes @math{O(log N)} time.

@examples(
  List.insert(["a", "b", "c"], 1, "x")
)

}


@doc(
  method (lst :: List).add(elem :: Any) :: List
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added to
 the end, equivalent to @rhombus(lst.insert(lst.length(), elem)).

@examples(
  List.add([2, 3], 1)
  [2, 3].add(1)
  block:
    let l = [2, 3]
    l.insert(l.length(), 1)
)

}


@doc(
  fun List.cons(elem :: Any, lst :: List) :: List
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added to
 the front, equivalent to @rhombus(lst.insert(0, elem)).

@examples(
  List.cons(1, [2, 3])
  [2, 3].insert(0, 1)
)

}

@doc(
  ~nonterminal:
    list_bind: def bind ~defn
    elem_bind: def bind ~defn
  bind.macro 'List.cons($elem_bind, $list_bind)'
){

 Matches a non-empty list where @rhombus(elem_bind) matches the
 first element of the list and @rhombus(list_bind) matches the
 rest of the list. Static information associated by @rhombus(List) is
 propagated to @rhombus(list_bind).

@examples(
  def List.cons(x, y) = [1, 2, 3]
  x
  y
)

}


@doc(
  def List.empty :: List = []
  bind.macro 'List.empty'
){

  A name and pattern for the empty list.

}


@doc(
  method (lst :: List).get(n :: NonnegInt) :: Any
){

 Equivalent to @rhombus(lst[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th element of
 @rhombus(lst) (starting from @rhombus(0)).
 Accessing a list element by position takes @math{O(log N)} time.

@examples(
  ["a", "b", "c"].get(1)
  ["a", "b", "c"][1]
)

}


@doc(
  method List.first(lst :: NonemptyList) :: Any
){

 Returns the first element of @rhombus(lst).
 Accessing the first element takes @math{O(log N)} time.

@examples(
  List.first(["a", "b", "c"])
  ["a", "b", "c"].first
)

}


@doc(
  method List.last(lst :: NonemptyList) :: Any
){

 Returns the last element of @rhombus(lst).
 Accessing the last element takes @math{O(log N)} time.

@examples(
  List.last(["a", "b", "c"])
  ["a", "b", "c"].last
)

}


@doc(
  method List.rest(lst :: NonemptyList) :: List
){

 Returns a list like @rhombus(lst), but without its first element.
 Creating the list takes @math{O(log N)} time.

@examples(
  List.rest(["a", "b", "c"])
  ["a", "b", "c"].rest
)

}


@doc(
  method (lst :: List).delete(n :: NonnegInt) :: List
){

 Creates a list like @rhombus(lst), but without the @rhombus(n)th
 element. The @rhombus(n) index must be less than the length of
 @rhombus(lst). Deletion takes @math{O(log N)} time.

@examples(
  List.delete(["a", "b", "c"], 1)
)

}

@doc(
  method (lst :: List).set(n :: NonnegInt, v :: Any) :: List
){

 Returns a list like @rhombus(lst), but with the @rhombus(n)th element
 replaced with @rhombus(v), equivalent to @rhombus(lst.delete(n).insert(n, v)).
 Note that this function performs a functional update, as opposed to
 mutable, which means lists do @emph{not} implement
 @rhombus(MutableIndexable, ~annot).

@examples(
  ["a", "b", "c"].set(1, "beta")
  ["a", "b", "c"].delete(1).insert(1, "beta")
)

}

@doc(
  method (lst :: List).length() :: NonnegInt
){

 Returns the number of items in @rhombus(lst).
 The length is produced in @math{O(1)} time.

@examples(
  [1, 4, 8].length()
  [].length()
)

}


@doc(
  method (lst :: List).reverse() :: List
){

 Returns a list with the same items as @rhombus(lst), but in reversed
 order. Reversing a list takes @math{O(N log N)} time, equivalent
 asymptotically to adding each element of @rhombus(lst) to another
 list one at a time.

@examples(
  [1, 4, 8].reverse()
)

}


@doc(
  method (lst :: List).append(lst :: List, ...) :: List
  fun List.append(lst :: List, ...) :: List
){

 Appends the @rhombus(lst)s in order. See also @rhombus(++).
 Appending takes @math{O(log N)} time, which is asymptotically faster than
 adding each element one at a time from one list to the other.

@examples(
  [1, 2, 3].append([4, 5], [6])
  List.append([1, 2, 3], [4, 5], [6])
  List.append()
)

}


@doc(
  method (lst :: List).take(n :: NonnegInt) :: List
  method (lst :: List).take_last(n :: NonnegInt) :: List
){

 Returns a list like @rhombus(lst), but with only the first @rhombus(n)
 elements in the case of @rhombus(List.take), or only the last
 @rhombus(n) elements in the case of @rhombus(List.take_last). The given
 @rhombus(lst) must have at least @rhombus(n) elements, otherwise an
 @rhombus(Exn.Fail.Contract, ~class) exception is thrown.
 Creating the new list takes @math{O(log N)} time, which is
 asymptotically faster than building a new list by adding or removing
 elements one at a time.

@examples(
  [1, 2, 3, 4, 5].take(2)
  [1, 2, 3, 4, 5].take_last(2)
  ~error:
    [1].take(2)
)

}


@doc(
  method (lst :: List).drop(n :: NonnegInt) :: List
  method (lst :: List).drop_last(n :: NonnegInt) :: List
){

 Returns a list like @rhombus(lst), but without the first @rhombus(n)
 elements in the case of @rhombus(List.drop), or without the last
 @rhombus(n) elements in the case of @rhombus(List.drop_last). The given
 @rhombus(lst) must have at least @rhombus(n) elements, otherwise an
 @rhombus(Exn.Fail.Contract, ~class) exception is thrown.
 Creating the new list takes @math{O(log N)} time, which is
 asymptotically faster than building a new list by adding or removing
 elements one at a time.

@examples(
  [1, 2, 3, 4, 5].drop(2)
  [1, 2, 3, 4, 5].drop_last(2)
  ~error:
    [1].drop(2)
)

}

@doc(
  method (lst :: List).sublist(n :: NonnegInt, m :: NonnegInt) :: List
){

 Returns a sublist of @rhombus(lst) containing elements from index
 @rhombus(n) (inclusive) to @rhombus(m) (exclusive), equivalent to
 @rhombus(lst.drop(n).take(m-n)).

@examples(
  [1, 2, 3, 4, 5].sublist(1, 3)
  [1, 2, 3, 4, 5].drop(1).take(3-1)
)


}


@doc(
  method (lst :: List).has_element(v :: Any,
                                   eqls :: Function.of_arity(2) = (_ == _))
    :: Boolean
){

 Returns @rhombus(#true) if @rhombus(lst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise, where @rhombus(eqls) determines
 equality. Searching the list takes @math{O(N)} time (multiplied by the
 cost of @rhombus(eqls)) to find an element as position @math{N}.

@examples(
  [1, 2, 3].has_element(2)
  [1, 2, 3].has_element(200)
)

}


@doc(
  method (lst :: List).find(pred :: Function.of_arity(1)) :: Any
){

 Returns the first element of @rhombus(lst) for which @rhombus(pred)
 returns true, @rhombus(#false) otherwise. Searching the list
 takes @math{O(N)} time (multiplied by the cost of @rhombus(pred))
 to find an element as position @math{N}.

@examples(
  [1, 2, 3].find((_ mod 2 .= 0))
  [1, 2, 3].find((_ mod 10 .= 9))
)

}


@doc(
  method (lst :: List).remove(v :: Any) :: List
){

 Returns a list like @rhombus(lst), but with the first element equal to
 @rhombus(v) (if any) removed.  Searching the list takes @math{O(N)} time.

@examples(
  [1, 2, 3, 2].remove(2)
)

}

@doc(
  method (lst :: List).map(f :: Function.of_arity(1))
    :: List,
  method (lst :: List).for_each(f :: Function.of_arity(1))
    :: Void,
){

 Like @rhombus(Function.map) and @rhombus(Function.for_each), but with a
 single list of arguments first, with the function supplied second.

@examples(
  List.map([1, 2, 3], (_ + 1))
  [1, 2, 3].map((_ + 1))
  [1, 2, 3].for_each(println)
)

}


@doc(
  method (lst :: List).sort(is_less :: Function.of_arity(2) = (_ < _))
    :: List,
){

 Sorts @rhombus(lst) using @rhombus(is_less) to compare elements.
 Sorting takes @math{O(N log N)} time.

@examples(
  List.sort([1, 3, 2])
  List.sort([1, 3, 2], (_ > _))
)

}


@doc(
  fun List.iota(n :: NonnegInt) :: List.of(NonnegInt)
){

 Returns a list containing the integers 0 to @rhombus(n) (exclusive) in
 order.

@examples(
  List.iota(3)
  List.iota(0)
)

}


@doc(
  method (lst :: List).copy() :: MutableList
){

 Equivalent to @rhombus(MutableList(& lst)), creates a new @tech{mutable
  list} with the same elements as @rhombus(lst).

@examples(
  [1, 2, 3].copy()
)

}


@doc(
  method (lst :: List).to_list() :: List
){

 Implements @rhombus(Listable, ~class) by returning @rhombus(lst) unchanged.

}


@doc(
  method (lst :: List).to_sequence() :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(lst)'s elements in order.

}


@doc(
  ~nonterminal:
    list_expr: block expr
  repet.macro 'List.repet($list_expr)'
){

 Creates a repetition from a list. This is a shorthand for using
 @rhombus(..., ~bind) with a @rhombus(List, ~bind) binding.

@examples(
  def lst = [1, 2, 3]
  block:
    let [x, ...] = lst
    [x+1, ...]
  [List.repet(lst) + 1, ...]
)

}
