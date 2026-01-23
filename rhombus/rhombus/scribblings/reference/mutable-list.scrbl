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
and it also supports element assignment via @brackets and @rhombus(:=).
A mutable list supports @tech{membership tests} using the @rhombus(in)
operator. A mutable list can be used as @tech{sequence}, in which case
it supplies its elements in order.

Operations on a mutable list tend to modify the mutable list and produce
@rhombus(#void), instead of creating a new list or returning the
modified mutable list.

The model of a mutable list as an object containing a list explains its
behavior in the case of concurrent modification: concurrent element
assignments for different positions will not interfere, but races with
other operations will sometimes negate one of the modifications.
Concurrent modification is thus somewhat unpredictable but still safe,
and it is not managed by a lock.

Two mutable lists are equal by @rhombus(is_now) if they have the same
length and their elements are pairwise equal by @rhombus(is_now). A
mutable list is never equal by @rhombus(is_now) or @rhombus(==) to an
immutable list.

@doc(
  annot.macro 'MutableList'
  annot.macro 'MutableList.now_of($annot)'
  annot.macro 'MutableList.later_of($annot)'
){

 Matches any mutable list in the form without @rhombus(now_of, ~datum)
 or @rhombus(later_of, ~datum).

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
 mutable list might change an element to one that does not satisfy
 @rhombus(annot).) Static information from @rhombus(annot) is propagated
 to accesses of the mutable list. Note that a converter @rhombus(annot) is
 applied for each access or update.

 Note that @rhombus(Any.like_element, ~annot) will not find any static
 information for elements from an expression with an
 @rhombus(MutableList, ~annot) or @rhombus(MutableList.now_of, ~annot)
 annotation, but an @rhombus(MutableList.later_of, ~annot) annotation can
 imply static information for elements.

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
    def l :: MutableList.later_of(Number) = MutableList(1, "b", 3)
  ~repl:
    l[0]
    ~error:
      l[1]
    ~error:
      l[2] := "c"
)

}

@doc(
  ~nonterminal:
    listable_expr: block expr
  fun MutableList(v :: Any, ...) :: MutableList
  expr.macro 'MutableList[$expr_or_splice, ...]'
  repet.macro 'MutableList[$repet_or_splice, ...]'

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

 Constructs a mutable list of the given @rhombus(v)s values or results of the
 @rhombus(expr_or_splice)s. A @rhombus(&) or @dots_expr form
 can appear within @brackets to splice a @tech{repetition} or existing @tech{listable}
 value into the constructed list, the same as in a function call (see
 @rhombus(#%call)). Mutable-list constructions can also serve as
 repetitions, where @rhombus(repet_or_splice) is like
 @rhombus(expr_or_splice), but with repetitions in place of expressions.

@examples(
  def l = MutableList(1, 2, 3)
  l
  l[0]
  l[0] := 10
  l
  MutableList.snapshot(l)
)

}

@doc(
  method (mlst :: MutableList).insert(n :: Nat, elem :: Any)
    :: Void
){

 Modifies @rhombus(mlst) to add @rhombus(elem) before the @rhombus(n)th
 element or at the end if @rhombus(n) is the length of @rhombus(lst). The
 @rhombus(n) index must be no more than the length of @rhombus(lst).
 Insertion takes @math{O(log N)} time.

@examples(
  def l = MutableList["a", "b", "c"]
  MutableList.insert(l, 1, "x")
  l
)

}


@doc(
  method (mlst :: MutableList).add(elem :: Any) :: Void
){

 Modifies @rhombus(mlst) to add @rhombus(elem) to
 the end, equivalent to @rhombus(mlst.insert(mlst.length(), elem)).

@examples(
  def l = MutableList[2, 3]
  MutableList.add(l, 1)
  l
  l.add(0)
  l
  l.insert(l.length(), 10)
  l
)

}


@doc(
  fun MutableList.cons(elem :: Any, mlst :: MutableList) :: Void
){

 Modifies @rhombus(mlst) to add @rhombus(elem) before all current elements,
 equivalent to @rhombus(mlst.insert(0, elem)).

@examples(
  def l = MutableList[2, 3]
  MutableList.cons(1, l)
  l
)

}

@doc(
  method (mlst :: MutableList).get(n :: Nat)
    :: Any.like_element(mlst)
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
  method (mlst :: MutableList).set(n :: Nat, v :: Any)
    :: Void
){

 Equivalent to @rhombus(mlst[n] := v) (with the default implicit
 @rhombus(#%index) form). Modifies @rhombus(mlst) to replace the
 @rhombus(n)th element with @rhombus(v). The @rhombus(n) index must be no
 more than the length of @rhombus(lst). Setting an element takes
 @math{O(log N)} time.

@examples(
  def l = MutableList["a", "b", "c"]
  l.set(1, "beta")
  l
  l[2] := "gamma"
  l
)

}


@doc(
  method (mlst :: MutableList).delete(n :: Nat)
    :: Void
){

 Modifies @rhombus(mlst) to delete the @rhombus(n)th
 element. The @rhombus(n) index must be less than the length of
 @rhombus(mlst). Deletion takes @math{O(log N)} time.

@examples(
  def l = MutableList["a", "b", "c"]
  MutableList.delete(l, 1)
  l
)

}

@doc(
  method (mlst :: MutableList).length() :: Nat
){

 Returns the number of items in @rhombus(mlst).
 The length is produced in @math{O(1)} time.

@examples(
  MutableList[1, 4, 8].length()
  MutableList[].length()
)

}


@doc(
  method (mlst :: MutableList).reverse() :: Void
){

 Modifies @rhombus(mlst) to have the same elements but in reversed
 order. Reversing a list takes @math{O(N log N)} time, equivalent
 asymptotically to adding each element of @rhombus(mlst) to another
 list one at a time.

@examples(
  def l = MutableList[1, 4, 8]
  l.reverse()
  l
)

}


@doc(
  method (mlst :: MutableList).append(lst :: List || MutableList)
    :: Void
){

 Modifies (mslt) to addend the elements of @rhombus(lst) in order.
 Appending @math{N} elements takes @math{O(N)} time.

@examples(
  def l = MutableList[1, 2, 3]
  MutableList.append(l, [4, 5])
  MutableList.append(l, MutableList[6])
  l
)

}


@doc(
  method (mlst :: MutableList).take(n :: Nat)
    :: Void
  method (mlst :: MutableList).take_last(n :: Nat)
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
  def l = MutableList[1, 2, 3, 4, 5]
  l.take(3)
  l
  l.take_last(2)
  l
  ~error:
    l.take(10)
)

}


@doc(
  method (mlst :: MutableList).drop(n :: Nat)
    :: Void
  method (mlst :: MutableList).drop_last(n :: Nat)
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
  def l = MutableList[1, 2, 3, 4, 5]
  l.drop(2)
  l
  l.drop_last(1)
  l
  ~error:
    l.drop(10)
)

}

@doc(
  method (mlst :: MutableList).sublist(rge :: Range)
    :: Void
  method (mlst :: MutableList).sublist(start :: Nat,
                                       end :: Nat)
    :: Void
){

 When given two arguments, modifies @rhombus(mlst) to keep only
 elements from index @rhombus(start) (inclusive) to @rhombus(end)
 (exclusive), equivalent to @rhombus(mlst.drop(start)) followed by
 @rhombus(mlst.take(end - start)).

 When given one argument, @rhombus(rge) is used to derive
 @rhombus(start) and @rhombus(end) as in @rhombus(String.substring).

@examples(
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.sublist(1, 3)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.drop(1)
    l.take(3-1)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.sublist(1..=3)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.sublist(1..)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.sublist(..3)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.sublist(..)
    l
)

}


@doc(
  method (lst :: MutableList).slice(start :: Int,
                                    end :: Int = lst.length())
    :: Void
){

 Similar to @rhombus(MutableList.sublist) with integer arguments, but
 when @rhombus(start) or @rhombus(end) is negative, it is replaced by
 @rhombus(lst.length()+start) or @rhombus(lst.length()+end),
 respectively.

@examples(
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.slice(1, 3)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.slice(1)
    l
  ~repl:
    def l = MutableList[1, 2, 3, 4, 5]
    l.slice(1, -2)
    l
)

}

@doc(
  method (mlst :: MutableList).contains(v :: Any,
                                        eqls :: Function.of_arity(2) = (_ == _))
    :: Boolean
){

 Returns @rhombus(#true) if @rhombus(mlst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise, where @rhombus(eqls) determines
 equality. Searching the list takes @math{O(N)} time (multiplied by the
 cost of @rhombus(eqls)) to find an element at position @math{N}. See
 also @rhombus(in).

@examples(
  def l = MutableList[1, 2, 3]
  l.contains(2)
  l.contains(200)
  l.contains(-2, (fun (a, b): math.abs(a) == math.abs(b)))
  2 in l
)

}


@doc(
  method (lst :: MutableList).index(v :: Any,
                                    eqls :: Function.of_arity(2) = (_ == _))
    :: maybe(Int)
){

 Like @rhombus(MutableList.contains), but instead of returning @rhombus(#true),
 returns the index of a found element.

@examples(
  def l = MutableList["a", "b", "c"]
  l.index("b")
  l.index("d")
  l.index("B", fun (a, b): Char.downcase(a[0]) == Char.downcase(b[0]))
)

}


@doc(
  method (lst :: MutableList).find(pred :: Function.of_arity(1))
    :: Any.like_element(mlst)
  method (lst :: MutableList).find_index(pred :: Function.of_arity(1))
    :: maybe(Nat)
){

 Like @rhombus(List.find) and @rhombus(List.index), but for
 @tech{mutable lists}. Searching the list takes @math{O(N)} time
 (multiplied by the cost of @rhombus(pred)) to find an element at
 position @math{N}.

@examples(
  def l = MutableList[1, 2, 3]
  l.find((_ mod 2 .= 0))
  l.find((_ mod 10 .= 9))
  l.find_index((_ mod 2 .= 0))
  l.find_index((_ mod 10 .= 9))
)

}


@doc(
  method (mlst :: MutableList).remove(v :: Any) :: Void
){

 Modifies @rhombus(mlst) to remove the first element equal to
 @rhombus(v) (if any).  Searching the list takes @math{O(N)} time.

@examples(
  def l = MutableList[1, 2, 3, 2]
  l.remove(2)
)

}

@doc(
  method (mlst :: MutableList).map(f :: Function.of_arity(1))
    :: Void,
  method (mlst :: MutableList).for_each(f :: Function.of_arity(1))
    :: Void,
){

 Similar to @rhombus(List.map) and @rhombus(List.for_each), but
 @rhombus(MutableList.map) modifies the mutable list to contain resulting
 items instead of returning a new list.

@examples(
  def l = MutableList[1, 2, 3]
  MutableList.map(l, (_ + 1))
  l
  l.for_each(println)
)

}


@doc(
  method (mlst :: MutableList).filter(
    ~keep: keep_pred :: Function.of_arity(1)
             = fun (_): #true,
    ~skip: skip_pred :: Function.of_arity(1)
             = fun (_): #false,
  ) :: Void
){

 Like @rhombus(List.filter), but modifies @rhombus(mlst) by dropping
 each element for which @rhombus(keep_pred) returns @rhombus(#false) or
 @rhombus(skip_pred) returns a true value.

@examples(
  ~repl:
    def l = MutableList[1, -1, -2, 2]
    l.filter(~keep: (_ > 0))
    l
  ~repl:
    def l = MutableList[1, -1, -2, 2]
    l.filter(~skip: (_ > 0))
    l
  ~repl:
    def l = MutableList[1, -1, -2, 2]
    l.filter(~keep: (_ != -2), ~skip: (_ > 0))
    l
)

}


@doc(
  method (mlst :: MutableList).sort(is_less :: Function.of_arity(2) = (_ < _))
    :: Void,
){

 Modifies @rhombus(mlst) to contain its elements sorted using
 @rhombus(is_less) to compare elements, Sorting takes @math{O(N log N)}
 time.

@examples(
  def l = MutableList[1, 3, 2]
  MutableList.sort(l)
  l
  MutableList.sort(l, (_ > _))
  l
)

}


@doc(
  method (mlst :: MutableList).copy() :: MutableList
){

 Equivalent to @rhombus(MutableList[& mlst]), creates a new @tech{mutable
  list} with the same elements as @rhombus(mlst).

@examples(
  MutableList[1, 2, 3].copy()
)

}


@doc(
  method (mlst :: MutableList).to_list()
    :: List.of(Any.like_element(mlst))
  method (mlst :: MutableList).snapshot()
    :: List.of(Any.like_element(mlst))
){

 Implements @rhombus(Listable, ~class) by returning a list containing
 the elements of @rhombus(mlst).

@examples(
  MutableList[1, 2, 3].to_list()
  [& MutableList[1, 2, 3]]
  MutableList[1, 2, 3].snapshot()
)

}


@doc(
  method (mlst :: MutableList).to_sequence()
    :: Sequence.expect_of(Any.like_element(mlst))
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(mlst)'s elements in order.

@examples(
  :
    for List (v in MutableList[1, 2, 3]): // optimizing
      v
  :
    for List (v in MutableList[1, 2, 3].to_sequence()): // non-optimizing
      v
)

}
