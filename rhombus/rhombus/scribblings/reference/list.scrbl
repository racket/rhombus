#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    lib("racket/treelist.rkt").treelist)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Lists}

A @deftech{list}@intro_note("list", "lists") is an ordered collection of elements, where a value can
appear multiple times as elements of a list. A list's length is the
number of elements that it contains. Most operations on
lists---including accessing an element, adding an element, appending
lists, or extracting a contiguous sublist---can be performed in
@math{O(log N)} time for a list of @math{N} elements. Lists are
immutable, so adding to a list or removing an element of a list produces
a new list without modifying the old one.

@margin_note_block{A Rhombus list is a Racket @rhombus(treelist) and not
 a Racket list.}

Lists can be constructed using the syntax
@rhombus([#,(@rhombus(expr, ~var)), ...]), which creates list containing the values of the
@rhombus(expr, ~var)s as elements. More precisely, a use of square
brackets without a preceding expression implicitly uses the
@rhombus(#%brackets) form, which is normally bound to
construct a list.

A list is @tech{indexable} using @brackets to access a list
element by position via
@rhombus(#%index). A list also works with the @rhombus(++) operator
to append lists. A list supports @tech{membership tests} using
the @rhombus(in) operator. A list can be used as @tech{sequence}, in which case
it supplies its elements in order.

Two lists are equal by @rhombus(==) if they have the same length and
their elements are pairwise equal by @rhombus(==).

@doc(
  annot.macro 'List'
  annot.macro 'List.of($annot)'
  annot.macro 'List.tuple_of[$annot, ..., $maybe_ellipsis]'
  annot.macro '#%brackets [$annot, ..., $maybe_ellipsis]'
  grammar maybe_ellipsis
  | #,(epsilon)
  | $ellipsis
  grammar ellipsis
  | #,(dots)
){

 The @rhombus(List, ~annot) annotation by itself matches any list. The
 @rhombus(List.of, ~annot) variant matches a list whose elements all satisfy
 @rhombus(annot).

 The @rhombus(List.tuple_of, ~annot) annotation matches a list whose
 elements each match the corresponding @rhombus(annot). If the last
 @rhombus(annot) is not followed by @rhombus(ellipsis), then the
 annotation matches only lists that have the same number of elements as
 the number of @rhombus(annot)s. If the last @rhombus(annot) is followed
 by @rhombus(ellipsis), then the annotation matches lists that have at
 least as many elements as @rhombus(annot)s except the last one, and all
 additional elements must match the last @rhombus(annot). For example,
 @rhombus(List.tuple_of[String, ...], ~annot) is the same as
 @rhombus(List.of(String), ~annot). If any @rhombus(annot) is a
 @tech(~doc: guide_doc){converter annotation}, then the
 @rhombus(List.tuple_of, ~annot) annotation is also a converter
 annotation where a converted list has converted elements. When all
 @rhombus(annot)s are @tech(~doc: guide_doc){predicate annotations}, then
 @rhombus(List.tuple_of[annot, ...], ~annot) is equivalent to
 @rhombus(matching(List[_ :: annot, ...]), ~annot).

 @see_implicit(@rhombus(#%brackets, ~annot), @brackets, "annotation")
 A @rhombus(#%brackets, ~annot) annotation is equivalent to a
 @rhombus(List.tuple_of, ~annot) annotation, so
 @rhombus([String, ...], ~annot) is also the same as
 @rhombus(List.of(String), ~annot).

 Static information associated by @rhombus(List, ~annot) or
 @rhombus(List.of, ~annot) makes an expression acceptable as a sequence
 to @rhombus(for) in static mode.

}

@doc(
  ~nonterminal:
    listable_expr: block expr
  fun List(v :: Any, ...) :: List.of(Any.like(v))
  expr.macro 'List[$expr_or_splice, ...]'
  repet.macro 'List[$repet_or_splice, ...]'
  expr.macro '#%brackets [$expr_or_splice, ...]'
  repet.macro '#%brackets [$repet_or_splice, ...]'

  grammar expr_or_splice
  | $expr
  | $repet #,(@litchar{,}) $ellipses
  | & $listable_expr

  grammar ellipses
  | $ellipsis
  | $ellipses #,(@litchar{,}) ellipsis

  grammar ellipsis
  | #,(dots_expr)
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
  bind.macro 'List[$bind_or_splice, ...]'
  bind.macro '#%brackets [$bind_or_splice, ...]'
  grammar bind_or_splice
  | $bind
  | $splice
  grammar splice
  | $repet_bind #,(@litchar{,}) $ellipsis
  | #,(@rhombus(&, ~bind)) $list_bind
  | #,(@rhombus(&, ~bind)) $list_repet_bind #,(@litchar{,}) $ellipsis
  grammar ellipsis
  | #,(dots)
  | #,(dots_expr) ~nonempty
  | #,(dots_expr) ~once
){

 Matches a list with as many elements as @rhombus(bind)s, or if a
 @rhombus(splice) is included, at least as many elements as
 @rhombus(bind)s. Each @rhombus(bind) matches an element of the list.
 Each @rhombus(splice) matches a sublist:
 @rhombus(repet_bind #,(@litchar{,}) ellipsis) matches a sublist of
 elements that individually match @rhombus(repet_bind),
 @rhombus(#,(@rhombus(&, ~bind)) list_bind) matches a sublist that itself
 matches @rhombus(list_bind), and
 @rhombus(#,(@rhombus(&, ~bind)) list_repet_bind #,(@litchar{,}) ellipsis)
 matches a sublist that is a concatenation of sublists that each match
 @rhombus(list_repet_bind). If @rhombus(ellipsis) includes @rhombus(~nonempty),
 then the matching sublist must have at least one element, otherwise the matching
 sublist can be empty. If @rhombus(ellipsis) includes @rhombus(~once),
 then a matching sublist has 0 or 1 elements.
 A matching sublist is deterministic if only one @rhombus(splice) is
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
  def List(1, x, ... ~nonempty, 3) = [1, 2, 3]
  [x, ...]
  ~error:
    def List(1, x, ... ~nonempty, 3) = [1, 3]
  def List(1, x, ... ~once, 3) = [1, 3]
  ~error:
    def List(1, x, ... ~once, 3) = [1, 2, 2, 3]
)

 If multiple @rhombus(splice)s are present, matching is greedy: the
 first @rhombus(splice) matches as many elements as possible to achieve a
 successful match, and so on for subsequent matches with the remaining
 elements.

@examples(
  def List(x, ..., z, ...) = [1, 2, "c", 5]
  [[x, ...], [z, ...]]
  def List(x, ..., z, ... ~nonempty) = [1, 2, "c", 5]
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
  method (lst :: List).insert(n :: Nat, elem :: Any)
    :: List.of(Any.like_element(lst) || Any.like(elem))
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
  method (lst :: List).add(elem :: Any)
    :: List.of(Any.like_element(lst) || Any.like(elem))
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
  fun List.cons(elem :: Any, lst :: List)
    :: List.of(Any.like_element(lst) || Any.like(elem))
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
  method (lst :: List).get(n :: Nat)
    :: Any.like_element(lst)
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
  property List.first(lst :: NonemptyList)
    :: Any.like_element(lst)
){

 Returns the first element of @rhombus(lst).
 Accessing the first element takes @math{O(log N)} time.

@examples(
  List.first(["a", "b", "c"])
  ["a", "b", "c"].first
)

}


@doc(
  property List.last(lst :: NonemptyList)
    :: Any.like_element(lst)
){

 Returns the last element of @rhombus(lst).
 Accessing the last element takes @math{O(log N)} time.

@examples(
  List.last(["a", "b", "c"])
  ["a", "b", "c"].last
)

}


@doc(
  property List.rest(lst :: NonemptyList)
    :: List.of(Any.like_element(lst))
){

 Returns a list like @rhombus(lst), but without its first element.
 Creating the list takes @math{O(log N)} time.

@examples(
  List.rest(["a", "b", "c"])
  ["a", "b", "c"].rest
)

}


@doc(
  method (lst :: List).delete(n :: Nat)
    :: List.of(Any.like_element(lst))
){

 Creates a list like @rhombus(lst), but without the @rhombus(n)th
 element. The @rhombus(n) index must be less than the length of
 @rhombus(lst). Deletion takes @math{O(log N)} time.

@examples(
  List.delete(["a", "b", "c"], 1)
)

}

@doc(
  method (lst :: List).set(n :: Nat, v :: Any)
    :: List.of(Any.like_element(lst) || Any.like(elemv))
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
  method (lst :: List).length() :: Nat
){

 Returns the number of items in @rhombus(lst).
 The length is produced in @math{O(1)} time.

@examples(
  [1, 4, 8].length()
  [].length()
)

}


@doc(
  method (lst :: List).reverse()
    :: List.of(Any.like_element(lst))
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
  method (lst :: List).append(lst2 :: List, ...)
    :: List.of(Any.like_element(lst) || Any.like_element(lst2))
  fun List.append(lst :: List, ...)
    :: List.of(Any.like_element(lst))
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
  method (lst :: List).take(n :: Nat)
    :: List.of(Any.like_element(lst))
  method (lst :: List).take_last(n :: Nat)
    :: List.of(Any.like_element(lst))
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
  method (lst :: List).drop(n :: Nat)
    :: List.of(Any.like_element(lst))
  method (lst :: List).drop_last(n :: Nat)
    :: List.of(Any.like_element(lst))
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
  method (lst :: List).sublist(rge :: Range)
    :: List.of(Any.like_element(lst))
  method (lst :: List).sublist(start :: Nat,
                               end :: Nat)
    :: List.of(Any.like_element(lst))
){

 When given two arguments, returns a sublist of @rhombus(lst)
 containing elements from index @rhombus(start) (inclusive) to
 @rhombus(end) (exclusive), equivalent to
 @rhombus(lst.drop(start).take(end - start)).

 When given one argument, @rhombus(rge) is used to derive
 @rhombus(start) and @rhombus(end) as in @rhombus(String.substring).

@examples(
  [1, 2, 3, 4, 5].sublist(1, 3)
  [1, 2, 3, 4, 5].drop(1).take(3-1)
  [1, 2, 3, 4, 5].sublist(1..=3)
  [1, 2, 3, 4, 5].sublist(1..)
  [1, 2, 3, 4, 5].sublist(..3)
  [1, 2, 3, 4, 5].sublist(..)
)


}


@doc(
  method (lst :: List).contains(v :: Any,
                                eqls :: Function.of_arity(2) = (_ == _))
    :: Boolean
){

 Returns @rhombus(#true) if @rhombus(lst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise, where @rhombus(eqls) determines
 equality. Searching the list takes @math{O(N)} time (multiplied by the
 cost of @rhombus(eqls)) to find an element at position @math{N}. See
 also @rhombus(in).

@examples(
  [1, 2, 3].contains(2)
  [1, 2, 3].contains(200)
  [10, 20, 30].contains(20.0, (_ .= _))
  2 in [1, 2, 3]
)

}


@doc(
  method (lst :: List).index(v :: Any,
                             eqls :: Function.of_arity(2) = (_ == _))
    :: maybe(Int)
){

 Like @rhombus(List.contains), but instead of returning @rhombus(#true),
 returns the index of a found element.

@examples(
  ["a", "b", "c"].index("b")
  ["a", "b", "c"].index("d")
  [10, 20, 30].index(20.0, (_ .= _))
)

}


@doc(
  method (lst :: List).find(pred :: Function.of_arity(1))
    :: Any.like_element(lst)
  method (lst :: List).find_index(pred :: Function.of_arity(1))
    :: maybe(Nat)
){

 The @rhombus(List.find) function finds the first element of
 @rhombus(lst) for which @rhombus(pred) returns a true value, or it
 returns @rhombus(#false) if no such element is found. The
 @rhombus(List.find_index) function is similar, but it returns the index of the
 found element instead of the element. Searching the list takes
 @math{O(N)} time (multiplied by the cost of @rhombus(pred)) to find an
 element at position @math{N}.

@examples(
  [1, 2, 3].find((_ mod 2 .= 0))
  [1, 2, 3].find((_ mod 10 .= 9))
  [1, 2, 3].find_index((_ mod 2 .= 0))
  [1, 2, 3].find_index((_ mod 10 .= 9))
)

}


@doc(
  method (lst :: List).remove(v :: Any)
    :: List.of(Any.like_element(lst))
){

 Returns a list like @rhombus(lst), but with the first element equal to
 @rhombus(v) (if any) removed.  Searching the list takes @math{O(N)} time.

@examples(
  [1, 2, 3, 2].remove(2)
)

}

@doc(
  method (lst :: List).map(f :: Function.of_arity(1))
    :: List.of(Any.like_result(f)),
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
  method (lst :: List).filter(
    ~keep: keep_pred :: Function.of_arity(1)
             = fun (_): #true,
    ~skip: skip_pred :: Function.of_arity(1)
             = fun (_): #false,
  ) :: List.of(Any.like_element(lst))
  method (lst :: List).partition(pred :: Function.of_arity(1))
    :: values(List.of(Any.like_element(lst)),
              List.of(Any.like_element(lst)))
){

 The @rhombus(List.filter) function returns a list that is like
 @rhombus(lst), but drops any element for which @rhombus(keep_pred)
 returns @rhombus(#false) or @rhombus(skip_pred) returns a true value. If
 @rhombus(keep_pred) returns @rhombus(#false) for a value, then
 @rhombus(skip_pred) is not called for that value.

 The @rhombus(List.partition) function returns two lists that are like
 @rhombus(lst), but with complementary elements: the first result list
 has elements for which @rhombus(pred) returns a true value, and the
 second result list has elements for which @rhombus(pred) returns
 @rhombus(#false).

@examples(
  [1, -1, -2, 2].filter(~keep: (_ > 0))
  [1, -1, -2, 2].filter(~skip: (_ > 0))
  [1, -1, -2, 2].filter(~keep: (_ != -2), ~skip: (_ > 0))
  [1, -1, -2, 2].partition((_ > 0))
)

}


@doc(
  method (lst :: List).sort(is_less :: Function.of_arity(2) = (_ < _))
    :: List.of(Any.like_element(lst))
){

 Sorts @rhombus(lst) using @rhombus(is_less) to compare elements.
 Sorting takes @math{O(N log N)} time.

@examples(
  List.sort([1, 3, 2])
  List.sort([1, 3, 2], (_ > _))
)

}


@doc(
  fun List.iota(n :: Nat) :: List.of(Nat)
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

 Equivalent to @rhombus(MutableList[& lst]), creates a new @tech{mutable
  list} with the same elements as @rhombus(lst).

@examples(
  [1, 2, 3].copy()
)

}


@doc(
  method (lst :: List).to_list()
    :: List.of(Any.like_element(lst))
){

 Implements @rhombus(Listable, ~class) by returning @rhombus(lst) unchanged.

@examples(
  [1, 2, 3].to_list()
  [& [1, 2, 3]]
)

}


@doc(
  method (lst :: List).to_sequence()
    :: Sequence.expect_of(Any.like_element(lst))
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(lst)'s elements in order.

@examples(
  :
    for List (v in [1, 2, 3]): // optimizing
      v
  :
    for List (v in [1, 2, 3].to_sequence()): // non-optimizing
      v
)

}
