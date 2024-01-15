#lang scribble/rhombus/manual
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
  @rhombus(List)
  [lst.length(), List.length(lst)]
  [lst.first, List.first(lst)]
  [lst.rest, List.rest(lst)]
  [lst.insert(lst, n, v), List.insert(lst, n, v)]
  [lst.add(lst, v), List.add(lst, v)]
  [lst.delete(lst, n), List.delete(lst, n)]
  [lst.reverse(), List.reverse(lst)]
  [lst.append(lst2, ...), List.append(lst, lst2, ...)]
  [lst.take_left(lst, n), List.take_left(lst, n)]
  [lst.take_right(lst, n), List.take_right(lst, n)]
  [lst.drop_left(lst, n), List.drop_left(lst, n)]
  [lst.drop_right(lst, n), List.drop_right(lst, n)]
  [lst.sublist(lst, n, m), List.sublist(lst, n, m)]
  [lst.has_element(lst, v), List.has_element(lst, v)]
  [lst.remove(lst, v), List.remove(lst, v)]
  [lst.map(func), List.map(lst, func)]
  [lst.for_each(func), List.for_each(lst, func)]
  [lst.sort(arg, ...), List.sort(lst, arg, ...)]
  [lst.to_list(), List.to_list(lst)]
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
  bind.macro 'List($bind, ..., $rest)'
  bind.macro '#%brackets [$bind, ...]'
  bind.macro '#%brackets [$bind, ..., $rest]'
  bind.macro 'List[$bind, ...]'
  bind.macro 'List[$bind, ..., $rest]'
  grammar rest:
    $repet_bind #,(@litchar{,}) $ellipsis
    & $list_bind
  grammar ellipsis:
    #,(dots)
){

 Matches a list with as many elements as @rhombus(bind)s, or if
 @rhombus(rest) is included, at least as many elements as
 @rhombus(bind)s, where the @rhombus(rest) (if present) matches the
 rest of the list.

 When @rhombus(& list_bind) is used, the rest of the list must match
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
 the end, equivalent to @rhombus(List.insert(ls, elem, List.length(lst))).

@examples(
  List.add([2, 3], 1)
)

}


@doc(
  fun List.cons(elem :: Any, lst :: List) :: List
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added to
 the front, equivalent to @rhombus(List.insert(ls, elem, 0)).

@examples(
  List.cons(1, [2, 3])
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
  fun List.first(lst :: NonemptyList)
){

 Returns the first element of @rhombus(lst).
 Accessing the first element takes @math{O(log N)} time.

@examples(
  List.first(["a", "b", "c"])
)

}

@doc(
  fun List.rest(lst :: NonemptyList) :: List
){

 Returns a list like @rhombus(lst), but without its first element.
 Creating the list takes @math{O(log N)} time.

@examples(
  List.rest(["a", "b", "c"])
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
  [1, 4, 8].length
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
  fun List.take_left(lst :: List, n :: NonnegInt) :: List
  fun List.take_right(lst :: List, n :: NonnegInt) :: List
){

 Returns a list like @rhombus(lst), but with only the first @rhombus(n)
 elements in the case of @rhombus(List.take_left), or only the last
 @rhombus(n) elements in the case of @rhombus(List.take_right). The given
 @rhombus(lst) must have at least @rhombus(n) elements, otherwise an
 @rhombus(Exn.Fail.Contract, ~class) exception is thrown.
 Creating the new list takes @math{O(log N)} time, which is
 asymptotically faster than building a new list by adding or removing
 elements one at a time.

@examples(
  [1, 2, 3, 4, 5].take_left(2)
  [1, 2, 3, 4, 5].take_right(2)
  ~error:
    [1].take_left(2)
)

}


@doc(
  fun List.drop_left(lst :: List, n :: NonnegInt) :: List
  fun List.drop_right(lst :: List, n :: NonnegInt) :: List
){

 Returns a list like @rhombus(lst), but without the first @rhombus(n)
 elements in the case of @rhombus(List.drop_left), or without the last
 @rhombus(n) elements in the case of @rhombus(List.drop_right). The given
 @rhombus(lst) must have at least @rhombus(n) elements, otherwise an
 @rhombus(Exn.Fail.Contract, ~class) exception is thrown.
 Creating the new list takes @math{O(log N)} time, which is
 asymptotically faster than building a new list by adding or removing
 elements one at a time.

@examples(
  [1, 2, 3, 4, 5].drop_left(2)
  [1, 2, 3, 4, 5].drop_right(2)
  ~error:
    [1].drop_left(2)
)

}

@doc(
  fun List.sublist(lst :: List, n :: NonnegInt, m :: NonnegInt) :: List
){

 Returns a sublist of @rhombus(lst) containing elements from index
 @rhombus(n) (inclusive) to @rhombus(m) (exclusive), equivalent to
 @rhombus(List.drop_left(lst, n).take_left(m-n)).

@examples(
  [1, 2, 3, 4, 5].sublist(1, 3)
)


}


@doc(
  fun List.has_element(lst :: List, v :: Any) :: Boolean
){

 Returns @rhombus(#true) if @rhombus(lst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise. Searching the list
 takes @math{O(N)} time.

@examples(
  [1, 2, 3].has_element(2)
  [1, 2, 3].has_element(200)
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
  List.map([1, 2, 3], fun (x): x + 1)
  [1, 2, 3].map(fun (x): x + 1)
  [1, 2, 3].for_each(println)
)

}


@doc(
  fun List.sort(lst :: List,
                is_less :: Function.of_arity(2) = math.less)
    :: List,
){

 Sorts @rhombus(lst) using @rhombus(is_less) to compare elements.
 Sorting takes @math{O(N log N)} time.

@examples(
  List.sort([1, 3, 2])
  List.sort([1, 3, 2], math.greater)
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
  fun List.to_list(lst :: List) :: List
){

 Implements @rhombus(Listable, ~class) by returning @rhombus(lst) unchanged.

}
