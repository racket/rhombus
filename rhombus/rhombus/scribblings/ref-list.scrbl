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

@dispatch_table(
  "list"
  List
  lst.length()
  lst.get(n)
  lst.set(n, v)
  lst.first,
  lst.last,
  lst.rest,
  lst.insert(n, v)
  lst.add(v)
  lst.delete(n)
  lst.reverse()
  lst.append(lst2, ...)
  lst.take(n)
  lst.take_last(n)
  lst.drop(n)
  lst.drop_last(n)
  lst.sublist(n, m)
  lst.has_element(v, eqls, ...)
  lst.find(pred)
  lst.remove(v)
  lst.map(func)
  lst.for_each(func)
  lst.sort(arg, ...)
  lst.copy()
  lst.to_list()
  lst.to_sequence()
)

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
    repet_bind: def bind ~defn
  bind.macro 'List($bind, ...)'
  bind.macro 'List($bind, ..., $rest, $bind, ...)'
  bind.macro '#%brackets [$bind, ...]'
  bind.macro '#%brackets [$bind, ..., $rest, $bind, ...]'
  bind.macro 'List[$bind, ...]'
  bind.macro 'List[$bind, ..., $rest, $bind, ...]'
  grammar rest:
    $repet_bind #,(@litchar{,}) $ellipsis
    & $list_bind
  grammar ellipsis:
    #,(dots)
){

 Matches a list with as many elements as @rhombus(bind)s, or if
 @rhombus(rest) is included, at least as many elements as
 @rhombus(bind)s, where the @rhombus(rest) (if present) matches the
 remainder of the list. Unlike most binding forms, a list pattern
 can include a @rhombus(rest) in the middle of a @rhombus(bind)
 sequence, and not just after @rhombus(bind)s.

 When @rhombus(& list_bind) is used, the remainder of the list must match
 the @rhombus(list_bind). Static information associated by
 @rhombus(List) is propagated to @rhombus(list_bind).

 When @rhombus(repet_bind) is used and does not impose a predicate or
 conversion on a matching value (e.g., @rhombus(repet_bind) is an
 identifier), then the corresponding elements of a matching value are not
 traversed, which means that matching can be constant-time. Using this
 repetition for the tail a new list similarly avoids traversing the
 elements.

 @see_implicit(@rhombus(#%brackets, ~bind), @brackets, "binding")

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

 A @tech{reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result list.

}

@doc(
  fun List.insert(lst :: List, n :: NonnegInt, elem :: Any) :: List
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
  fun List.add(lst :: List, elem :: Any) :: List
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
  fun List.get(lst :: List, n :: NonnegInt) :: Any
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
  fun List.first(lst :: NonemptyList) :: Any
){

 Returns the first element of @rhombus(lst).
 Accessing the first element takes @math{O(log N)} time.

@examples(
  List.first(["a", "b", "c"])
  ["a", "b", "c"].first
)

}


@doc(
  fun List.last(lst :: NonemptyList) :: Any
){

 Returns the last element of @rhombus(lst).
 Accessing the last element takes @math{O(log N)} time.

@examples(
  List.last(["a", "b", "c"])
  ["a", "b", "c"].last
)

}


@doc(
  fun List.rest(lst :: NonemptyList) :: List
){

 Returns a list like @rhombus(lst), but without its first element.
 Creating the list takes @math{O(log N)} time.

@examples(
  List.rest(["a", "b", "c"])
  ["a", "b", "c"].rest
)

}


@doc(
  fun List.delete(lst :: List, n :: NonnegInt) :: List
){

 Creates a list like @rhombus(lst), but without the @rhombus(n)th
 element. The @rhombus(n) index must be less than the length of
 @rhombus(lst). Deletion takes @math{O(log N)} time.

@examples(
  List.delete(["a", "b", "c"], 1)
)

}

@doc(
  fun List.set(lst :: List, n :: NonnegInt, v :: Any) :: List
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


@doc(
  fun List.length(lst :: List) :: NonnegInt
){

 Returns the number of items in @rhombus(lst).
 The length is produced in @math{O(1)} time.

@examples(
  List.length([1, 4, 8])
  List.length([])
  [1, 4, 8].length()
  [].length()
)

}


@doc(
  fun List.reverse(lst :: List) :: List
){

 Returns a list with the same items as @rhombus(lst), but in reversed
 order. Reversing a list takes @math{O(N log N)} time, equivalent
 asymptotically to adding each element of @rhombus(lst) to another
 list one at a time.

@examples(
  List.reverse([1, 4, 8])
  [1, 4, 8].reverse()
)

}


@doc(
  fun List.append(lst :: List, ...) :: List
){

 Appends the @rhombus(lst)s in order. See also @rhombus(++).
 Appending takes @math{O(log N)} time, which is asymptotically faster than
 adding each element one at a time from one list to the other.

@examples(
  List.append([1, 2, 3], [4, 5], [6])
  [1, 2, 3].append([4, 5], [6])
)

}


@doc(
  fun List.take(lst :: List, n :: NonnegInt) :: List
  fun List.take_last(lst :: List, n :: NonnegInt) :: List
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
  fun List.drop(lst :: List, n :: NonnegInt) :: List
  fun List.drop_last(lst :: List, n :: NonnegInt) :: List
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
  fun List.sublist(lst :: List, n :: NonnegInt, m :: NonnegInt) :: List
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
  fun List.has_element(lst :: List, v :: Any,
                       eqls :: Function.of_arity(2) = (_ == _))
    :: Boolean
){

 Returns @rhombus(#true) if @rhombus(lst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise, where @rhombus(eqls) determines
 equality. Searching the list takes @math{O(N)} time (multiplified by the
 cost of @rhombus(eqls)) to find an element as position @math{N}.

@examples(
  [1, 2, 3].has_element(2)
  [1, 2, 3].has_element(200)
)

}


@doc(
  fun List.find(lst :: List, pred :: Function.of_arity(1)) :: Any
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
  fun List.remove(lst :: List, v :: Any) :: List
){

 Returns a list like @rhombus(lst), but with the first element equal to
 @rhombus(v) (if any) removed.  Searching the list takes @math{O(N)} time.

@examples(
  [1, 2, 3, 2].remove(2)
)

}

@doc(
  fun List.map(lst :: List, f :: Function.of_arity(1))
    :: List,
  fun List.for_each(lst :: List, f :: Function.of_arity(1))
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
  fun List.sort(lst :: List,
                is_less :: Function.of_arity(2) = (_ < _))
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
  fun List.copy(lst :: List) :: MutableList
){

 Equivalent to @rhombus(MutableList(& lst)), creates a new @tech{mutable
  list} with the same elements as @rhombus(lst).

@examples(
  [1, 2, 3].copy()
)

}


@doc(
  fun List.to_list(lst :: List) :: List
){

 Implements @rhombus(Listable, ~class) by returning @rhombus(lst) unchanged.

}


@doc(
  fun List.to_sequence(lst :: List) :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(lst)'s elements in order.

}
