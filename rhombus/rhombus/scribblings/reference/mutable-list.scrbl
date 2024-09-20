#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title{Mutable Lists}

A @deftech{mutable list} is a mutable object that contains a
@tech{list}. A mutable list is not by itself a list (i.e., it will not
satisfy the @rhombus(List, ~annot) annotation), but it is
@tech{listable}.

A mutable list is @tech{indexable} using @brackets to access a list
element by position---in @math{O(log N)} time---via @rhombus(#%index),
and it also support element assignment via @brackets and @rhombus(:=). A
mutable list can be used as @tech{sequence}, in which case it supplies
its elements in order.

Operations on a mutable list tend to modify the mutable list and produce
@rhombus(#void), instead of creating a new list or returning the
modified mutable list.

The model of a mutable list as an object containing a list explains its
behavior in the case of concurrent modification: concurrent element
assignments for different positions will not interfere, but races with
other operations will sometimes negate one of the modifications.
Concurrent modification is thus somewhat unpredictable but still safe,
and it is not managed by a lock.

@dispatch_table(
  "mutable list"
  MutableList
  mlst.length()
  mlst.get(n)
  mlst.set(n, v)
  mlst.insert(n, v)
  mlst.add(v)
  mlst.delete(n)
  mlst.reverse()
  mlst.append(lst2)
  mlst.take(n)
  mlst.take_last(n)
  mlst.drop(n)
  mlst.drop_last(n)
  mlst.sublist(n, m)
  mlst.has_element(v, eqls, ...)
  mlst.find(pred)
  mlst.remove(v)
  mlst.map(func)
  mlst.for_each(func)
  mlst.sort(arg, ...)
  mlst.snapshot()
  mlst.to_list()
  mlst.to_sequence()
)

@doc(
  annot.macro 'MutableList'
  annot.macro 'MutableList.now_of($annot)'
  annot.macro 'MutableList.later_of($annot)'
){

 Matches any mutable list in the form without @rhombus(now_of) or @rhombus(later_of).

 The @rhombus(MutableList.now_of, ~annot) form constructs a
 @tech(~doc: guide_doc){predicate annotation} that matches a mutable list whose elements
 all currently satisfy @rhombus(annot), but it does not ensure in any way
 that future values installed into the mutable list will satisfy
 @rhombus(annot). The given @rhombus(annot) must not be a converting
 annotation. Static information from @rhombus(annot) is not propagated to
 accesses of the mutable list, since there's no guarantee that the value
 will still satisfy the annotation.

 The @rhombus(MutableList.later_of, ~annot) form constructs a
 @tech(~doc: guide_doc){converter annotation} that immediately matches a mutable list
 without checking that its elements currently satisfy @rhombus(annot).
 The conversion result of the annotation is a view of the original
 mutable list, but one where @rhombus(annot) is checked against a value
 that would be returned by accessing an element of the mutable list or a
 value to be installed into the mutable list. (A different view of the
 mutable list might change an element to one that does not astisfy
 @rhombus(annot).) Static information from @rhombus(annot) is propagated
 to accesses of the mutable list. Note that a converter @rhombus(annot) is
 applied for each access or update.

 Static information associated by @rhombus(MutableList, ~annot) or
 @rhombus(MutableList.now_of, ~annot) makes an expression acceptable as a
 sequence to @rhombus(for) in static mode.

@examples(
  ~repl:
    MutableList(1, 2, 3) :: MutableList
    MutableList(1, 2, 3) :: MutableList.now_of(Number)
    ~error:
      MutableList(1, "b", 3) :: MutableList.now_of(Number)
  ~defn:
    def a :: MutableList.later_of(Number) = MutableList(1, "b", 3)
  ~repl:
    a[0]
    ~error:
      a[1]
    ~error:
      a[2] := "c"
)

}

@doc(
  ~nonterminal:
    listable_expr: block expr
  fun MutableList(v :: Any, ...) :: MutableList
  expr.macro 'MutableList[$expr_or_splice, ...]'
  repet.macro 'MutableList[$repet_or_splice, ...]'

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

 Constructs a mutable list of the given @rhombus(v)s values or results of the
 @rhombus(expr_or_splice)s. A @rhombus(&) or @dots_expr form
 can appear within @brackets to splice a @tech{repetition} or existing @tech{listable}
 value into the constructed list, the same as in a function call (see
 @rhombus(#%call)). Mutable-list constructions can also serve as
 repetitions, where @rhombus(repet_or_splice) is like
 @rhombus(expr_or_splice), but with repetitions in place of expressions.

@examples(
  def mlst = MutableList(1, 2, 3)
  mlst
  mlst[0]
  mlst[0] := 10
  mlst
  MutableList.snapshot(mlst)
)

}

@doc(
  fun MutableList.insert(mlst :: MutableList,
                         n :: NonnegInt, elem :: Any)
    :: Void
){

 Modifies @rhombus(mlst) to add @rhombus(elem) before the @rhombus(n)th
 element or at the end if @rhombus(n) is the length of @rhombus(lst). The
 @rhombus(n) index must be no more than the length of @rhombus(lst).
 Insertion takes @math{O(log N)} time.

@examples(
  def mlst = MutableList["a", "b", "c"]
  MutableList.insert(mlst, 1, "x")
  mlst
)

}


@doc(
  fun MutableList.add(mlst :: MutableList, elem :: Any) :: Void
){

 Modifies @rhombus(mlst) to add @rhombus(elem) to
 the end, equivalent to @rhombus(mlst.insert(mlst.length(), elem)).

@examples(
  def mlst = MutableList[2, 3]
  MutableList.add(mlst, 1)
  mlst
  mlst.add(0)
  mlst
  mlst.insert(mlst.length(), 10)
  mlst
)

}


@doc(
  fun MutableList.cons(elem :: Any, mlst :: MutableList) :: Void
){

 Modifies @rhombus(mlst) to add @rhombus(elem) before all current elements,
 equivalent to @rhombus(mlst.insert(0, elem)).

@examples(
  def mlst = MutableList[2, 3]
  MutableList.cons(1, mlst)
  mlst
)

}

@doc(
  fun MutableList.get(mlst :: MutableList, n :: NonnegInt) :: Any
){

 Equivalent to @rhombus(mlst[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th element of
 @rhombus(mlst) (starting from @rhombus(0)).
 Accessing a list element by position takes @math{O(log N)} time.

@examples(
  MutableList["a", "b", "c"].get(1)
  MutableList["a", "b", "c"][1]
)

}

@doc(
  fun MutableList.set(mlst :: MutableList,
                      n :: NonnegInt, v :: Any)
    :: Void
){

 Equivalent to @rhombus(mlst[n] := v) (with the default implicit
 @rhombus(#%index) form). Modifies @rhombus(mlst) to replace the
 @rhombus(n)th element with @rhombus(v). The @rhombus(n) index must be no
 more than the length of @rhombus(lst). Setting an element takes
 @math{O(log N)} time.

@examples(
  def mlst = MutableList["a", "b", "c"]
  mlst.set(1, "beta")
  mlst
  mlst[2] := "gamma"
  mlst
)

}


@doc(
  fun MutableList.delete(mlst :: MutableList, n :: NonnegInt)
    :: Void
){

 Modifies @rhombus(mlst) to delete the @rhombus(n)th
 element. The @rhombus(n) index must be less than the length of
 @rhombus(mlst). Deletion takes @math{O(log N)} time.

@examples(
  def mlst = MutableList["a", "b", "c"]
  MutableList.delete(mlst, 1)
  mlst
)

}

@doc(
  fun MutableList.length(mlst :: MutableList) :: NonnegInt
){

 Returns the number of items in @rhombus(mlst).
 The length is produced in @math{O(1)} time.

@examples(
  MutableList.length(MutableList[1, 4, 8])
  MutableList.length(MutableList[])
  MutableList[1, 4, 8].length()
  MutableList[].length()
)

}


@doc(
  fun MutableList.reverse(mlst :: MutableList) :: Void
){

 Modifies @rhombus(mlst) to have the same elements but in reversed
 order. Reversing a list takes @math{O(N log N)} time, equivalent
 asymptotically to adding each element of @rhombus(mlst) to another
 list one at a time.

@examples(
  def mlst = MutableList[1, 4, 8]
  MutableList.reverse(mlst)
  mlst
)

}


@doc(
  fun MutableList.append(mlst :: MutableList,
                         lst :: List || MutableList)
    :: Void
){

 Modifies (mslt) to addend the elements of @rhombus(lst) in order.
 Appending @math{N} elements takes @math{O(N)} time.

@examples(
  def mlst = MutableList[1, 2, 3]
  MutableList.append(mlst, [4, 5])
  MutableList.append(mlst, MutableList[6])
  mlst
)

}


@doc(
  fun MutableList.take(mlst :: MutableList, n :: NonnegInt)
    :: Void
  fun MutableList.take_last(mlst :: MutableList, n :: NonnegInt)
    :: Void
){

 Modifies @rhombus(mlst) to keep only the first @rhombus(n)
 elements in the case of @rhombus(MutableList.take), or only the last
 @rhombus(n) elements in the case of @rhombus(MutableList.take_last). The given
 @rhombus(mlst) must have at least @rhombus(n) elements, otherwise an
 @rhombus(Exn.Fail.Contract, ~class) exception is thrown.
 Modifying the list takes @math{O(log N)} time, which is
 asymptotically faster than building a new list by adding or removing
 elements one at a time.

@examples(
  def mlst = MutableList[1, 2, 3, 4, 5]
  mlst.take(3)
  mlst
  mlst.take_last(2)
  mlst
  ~error:
    mlst.take(10)
)

}


@doc(
  fun MutableList.drop(mlst :: MutableList, n :: NonnegInt)
    :: Void
  fun MutableList.drop_last(mlst :: MutableList, n :: NonnegInt)
    :: Void
){

 Modifies @rhombus(mlst) to remove the first @rhombus(n)
 elements in the case of @rhombus(MutableList.drop), or the last
 @rhombus(n) elements in the case of @rhombus(MutableList.drop_last). The given
 @rhombus(mlst) must have at least @rhombus(n) elements, otherwise an
 @rhombus(Exn.Fail.Contract, ~class) exception is thrown.
 Modifying the list takes @math{O(log N)} time, which is
 asymptotically faster than building a new list by adding or removing
 elements one at a time.

@examples(
  def mlst = MutableList[1, 2, 3, 4, 5]
  mlst.drop(2)
  mlst
  mlst.drop_last(1)
  mlst
  ~error:
    mlst.drop(10)
)

}

@doc(
  fun MutableList.sublist(mlst :: MutableList,
                          n :: NonnegInt, m :: NonnegInt)
    :: Void
){

 Modifies @rhombus(mlst) to keep only elements from index
 @rhombus(n) (inclusive) to @rhombus(m) (exclusive), equivalent to
 @rhombus(mlst.drop(n)) followed by @rhombus(mlst.take(m-n)).

@examples(
  def mlst = MutableList[1, 2, 3, 4, 5]
  mlst.sublist(1, 3)
  mlst
)


}


@doc(
  fun MutableList.has_element(mlst :: MutableList, v :: Any,
                              eqls :: Function.of_arity(2) = (_ == _))
    :: Boolean
){

 Returns @rhombus(#true) if @rhombus(mlst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise, where @rhombus(eqls) determines
 equality. Searching the list takes @math{O(N)} time (multiplified by the
 cost of @rhombus(eqls)) to find an element as position @math{N}.

@examples(
  [1, 2, 3].has_element(2)
  [1, 2, 3].has_element(200)
)

}


@doc(
  fun MutableList.find(mlst :: MutableList,
                       pred :: Function.of_arity(1))
    :: Any
){

 Returns the first element of @rhombus(mlst) for which @rhombus(pred)
 returns true, @rhombus(#false) otherwise. Searching the list
 takes @math{O(N)} time (multiplied by the cost of @rhombus(pred))
 to find an element as position @math{N}.

@examples(
  [1, 2, 3].find((_ mod 2 .= 0))
  [1, 2, 3].find((_ mod 10 .= 9))
)

}


@doc(
  fun MutableList.remove(mlst :: MutableList, v :: Any) :: Void
){

 Modifies @rhombus(mlst) to remove the first element equal to
 @rhombus(v) (if any).  Searching the list takes @math{O(N)} time.

@examples(
  def mlst = MutableList[1, 2, 3, 2]
  mlst.remove(2)
)

}

@doc(
  fun MutableList.map(mlst :: MutableList,
                      f :: Function.of_arity(1))
    :: Void,
  fun MutableList.for_each(mlst :: MutableList,
                           f :: Function.of_arity(1))
    :: Void,
){

 Similar to @rhombus(List.map) and @rhombus(List.for_each), but
 @rhombus(MutableList.map) modifies the mutable list to contain resulting
 items instead of returning a new list.

@examples(
  def mlst = MutableList[1, 2, 3]
  MutableList.map(mlst, (_ + 1))
  mlst
  mlst.for_each(println)
)

}


@doc(
  fun MutableList.sort(mlst :: MutableList,
                       is_less :: Function.of_arity(2) = (_ < _))
    :: Void,
){

 Modifies @rhombus(mlst) to contain its elements sorted using
 @rhombus(is_less) to compare elements, Sorting takes @math{O(N log N)}
 time.

@examples(
  def mlst = MutableList[1, 3, 2]
  MutableList.sort(mlst)
  mlst
  MutableList.sort(mlst, (_ > _))
  mlst
)

}


@doc(
  fun MutableList.to_list(mlst :: MutableList) :: List
  fun MutableList.snapshot(mlst :: MutableList) :: List
){

 Implements @rhombus(Listable, ~class) by returning a list containing
 the elements of @rhombus(mlst).

}


@doc(
  fun MutableList.to_sequence(mlst :: MutableList) :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(mlst)'s elements in order.

}
