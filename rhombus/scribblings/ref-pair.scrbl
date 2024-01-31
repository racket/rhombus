#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title(~tag: "Pairs"){Pairs and Pair Lists}

A @deftech{pair} combines two values: a ``first'' value and a ``rest''
value. A @deftech{pair list} is a @tech{listable} value that is
constructed with pairs and the empty pair list; every non-empty pair
list is a pair, a pair is a pair list only if its ``rest'' is a list.

@dispatch_table(
  "pair"
  Pair
  pr.first
  pr.rest

)

@dispatch_table(
  "pait list"
  PairList
  lst.length()
  lst.get(n)
  lst.first,
  lst.rest,
  lst.reverse()
  lst.append(lst2, ...)
  lst.take_left(lst, n)
  lst.take_right(lst, n)
  lst.drop_left(lst, n)
  lst.drop_right(lst, n)
  lst.has_element(lst, v)
  lst.remove(lst, v)
  lst.map(func)
  lst.for_each(func)
  lst.sort(arg, ...)
  lst.to_list()
)

@doc(
  ~nonterminal:
    fst_annot: :: annot
    rst_annot: :: annot
  annot.macro 'Pair'
  annot.macro 'Pair.of($fst_annot, $rst_annot)'
){

 Matches any pair in the form without @rhombus(of). The @rhombus(of)
 variant matches a pair whose components satisfy @rhombus(fst_annot)
 and @rhombus(rst_annot).

}

@doc(
  fun Pair(fst_v :: Any, rst_v :: Any) :: Pair
){

 Constructs a pair containing @rhombus(fst_v) and @rhombus(rst_v).

@examples(
  def pr = Pair(1, 2)
  pr
  pr.first
  pr.rest
)

}

@doc(
  ~nonterminal:
    fst_bind: def bind ~defn
    rst_bind: def bind ~defn
  bind.macro 'Pair($fst_bind, $rst_bind)'
){

 Matches a pair whose first component matches @rhombus(fst_bind) and
 rest component matches @rhombus(rst_bind).

@examples(
  def Pair(x, y) = Pair(1, 2)
  y
  ~error:
    def Pair(sx :: String, sy :: String) = Pair(1, 2)
  def Pair(lx, ly) = PairList[1, 2]
  y
)

}

@doc(
  ~nonterminal:
    fst_bind: def bind ~defn
    rst_bind: def bind ~defn
  fun Pair.cons(fst :: Any, rst :: Any) :: Pair
  bind.macro 'Pair.cons($fst_bind, $rst_bind)'
){

 Aliases for @rhombus(Pair) in expression and binding positions.

 Note that the difference between @rhombus(Pair.cons) and
 @rhombus(PairList.cons) is that @rhombus(PairList.cons) requires a list as is
 second argument, which means that it always forms a @tech{list}. In
 contrast, @rhombus(Pair.cons) allows any value as its second
 argument---but it creates a list if that argument is a list.

@examples(
  Pair.cons(1, 2)
  Pair.cons(1, [2, 3])
)

}

@doc(
  fun Pair.first(pr :: Pair)
  fun Pair.rest(pr :: Pair)
){

 Returns the first or second component of @rhombus(pr).

@examples(
  Pair.first(Pair("a", "b"))
  Pair.rest(Pair("a", "b"))
)

}


@doc(
  annot.macro 'PairList'
  annot.macro 'PairList.of($annot)'
){

 Matches any @tech{pair list} in the form without @rhombus(of). The @rhombus(of)
 variant matches a pair list whose elements satisfy @rhombus(annot).

 Static information associated by @rhombus(PairList, ~annot) or
 @rhombus(PairList.of, ~annot) makes an expression acceptable as a sequence
 to @rhombus(for) in static mode.

}

@doc(
  ~nonterminal:
    listable_expr: block expr
  fun PairList(v :: Any, ...) :: PairList
  expr.macro 'PairList[$expr_or_splice, ...]'
  repet.macro 'PairList[$repet_or_splice, ...]'

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

 Constructs a @tech{pair list} of the given @rhombus(v)s values or results of the
 @rhombus(expr_or_splice)s. A @rhombus(&) or @dots_expr form
 can appear within @brackets to splice a @tech{repetition} or existing @tech{listable}
 value into the constructed pair list, the same as in a function call (see
 @rhombus(#%call)). Pair-list constructions can also serve as
 repetitions, where @rhombus(repet_or_splice) is like
 @rhombus(expr_or_splice), but with repetitions in place of expressions.

 @see_implicit(@rhombus(#%brackets), @brackets, "expression")

@examples(
  def lst = PairList(1, 2, 3)
  lst
  lst[0]
  lst ++ PairList[4, 5]
  PairList[1, 2, 3]
)

}

@doc(
  ~nonterminal:
    pair_list_bind: def bind ~defn
    repet_bind: def bind ~defn
  bind.macro 'PairList($bind, ...)'
  bind.macro 'PairList($bind, ..., $rest)'
  bind.macro 'PairList[$bind, ...]'
  bind.macro 'PairList[$bind, ..., $rest]'
  grammar rest:
    $repet_bind #,(@litchar{,}) $ellipsis
    & $pair_list_bind
  grammar ellipsis:
    #,(dots)
){

 Matches a @tech{pair list} with as many elements as @rhombus(bind)s, or if
 @rhombus(rest) is included, at least as many elements as
 @rhombus(bind)s, where the @rhombus(rest) (if present) matches the
 rest of the pair list.

 When @rhombus(& pair_list_bind) is used, the rest of the list must match
 the @rhombus(pair_list_bind). Static information associated by
 @rhombus(PairList) is propagated to @rhombus(pair_list_bind).

 When @rhombus(repet_bind) is used and does not impose a predicate or
 conversion on a matching value (e.g., @rhombus(repet_bind) is an
 identifier), then the corresponding elements of a matching value are not
 traversed, which means that matching can be constant-time. Using this
 repetition for the tail a new list similarly avoids traversing the
 elements.

 @see_implicit(@rhombus(#%brackets, ~bind), @brackets, "binding")

@examples(
  def PairList(1, x, y) = PairList[1, 2, 3]
  y
  def PairList[1, also_x, also_y] = PairList[1, 2, 3]
  also_y
  def PairList(1, & xs) = PairList[1, 2, 3]
  xs
  def PairList(1, x, ...) = PairList[1, 2, 3]
  PairList[x, ...]
)

}

@doc(
  annot.macro 'NonemptyPairList'
  annot.macro 'NonemptyPairList.of($annot)'
){

 Like @rhombus(PairList, ~annot) as an annotation, but matches only non-empty
 pair lists.

@examples(
  PairList[1] :: NonemptyPairList
  ~error:
    PairList[] :: NonemptyPairList
)

}

@doc(
  reducer.macro 'PairList'
){

 A @tech{reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result @tech{pair list}.

}

@doc(
  fun PairList.cons(elem :: Any, lst :: PairList) :: PairList
){

 Creates a list like @rhombus(lst), but with @rhombus(elem) added to the
 front. Adding a element to the front for a pair list takes @math{O(1)}
 time.

@examples(
  PairList.cons(1, PairList[2, 3])
)

}

@doc(
  ~nonterminal:
    list_bind: def bind ~defn
    elem_bind: def bind ~defn
  bind.macro 'PairList.cons($elem_bind, $list_bind)'
){

 Matches a non-empty pair list where @rhombus(elem_bind) matches the
 first element of the pair list and @rhombus(list_bind) matches the
 rest of the pair list. Static information associated by @rhombus(PairList) is
 propagated to @rhombus(list_bind).

@examples(
  def Pair.cons(x, y) = PairList[1, 2, 3]
  x
  y
)

}


@doc(
  def PairList.empty :: PairList = PairList[]
  bind.macro 'PairList.empty'
){

  A name and pattern for the empty pair list.

}


@doc(
  fun PairList.get(lst :: PairList, n :: NonnegInt) :: Any
){

 Equivalent to @rhombus(lst[n]) (with the default implicit
 @rhombus(#%index) form). Returns the @rhombus(n)th element of
 @rhombus(lst) (starting from @rhombus(0)).
 Accessing a pair list element by position takes @math{O(N)} time.

@examples(
  PairList["a", "b", "c"].get(1)
  PairList["a", "b", "c"][1]
)

}


@doc(
  fun PairList.first(lst :: NonemptyPairList) :: Any
){

 Returns the first element of @rhombus(lst).
 Accessing the first element takes @math{O(1)} time.

@examples(
  PairList.first(PairList["a", "b", "c"])
)

}

@doc(
  fun PairList.rest(lst :: NonemptyPairList) :: PairList
){

 Returns a @tech{pair list} like @rhombus(lst), but without its first element.
 For a given pair list, @rhombus(List.rest) always produces the same value
 in the sense of @rhombus(===), and accessing that values takes @math{O(1)} time.

@examples(
  PairList.rest(PairList["a", "b", "c"])
)

}


@doc(
  ~nonterminal:
    pair_list_expr: block expr
  repet.macro 'PairList.repet($list_expr)'
){

 Creates a repetition from a @tech{pair list}. This is a shorthand for using
 @rhombus(..., ~bind) with a @rhombus(PairList, ~bind) binding.

@examples(
  def lst = PairList[1, 2, 3]
  block:
    let PairList[x, ...] = lst
    PairList[x+1, ...]
  [PairList.repet(lst) + 1, ...]
)

}


@doc(
  fun PairList.length(lst :: PairList) :: NonnegInt
){

 Returns the number of items in @rhombus(lst).
 Computing this length takes @math{O(N)} time.

@examples(
  PairList.length(PairList[1, 4, 8])
  PairList.length(PairList[])
  PairList[1, 4, 8].length
)

}


@doc(
  fun PairList.reverse(lst :: PairList) :: PairList
){

 Returns a @tech{pair list} with the same items as @rhombus(lst), but in reversed
 order. Reversing a list takes @math{O(N)} time.

@examples(
  PairList.reverse(PairList[1, 4, 8])
  PairList[1, 4, 8].reverse()
)

}


@doc(
  fun PairList.append(lst :: PairList, ...) :: PairList
){

 Appends the @rhombus(lst)s in order. See also @rhombus(++).
 Appending takes @math{O(N)} time.

@examples(
  PairList.append(PairList[1, 2, 3], PairList[4, 5], PairList[6])
  PairList[1, 2, 3].append(PairList[4, 5], PairList[6])
)

}


@doc(
  fun PairList.take_left(lst :: PairList, n :: NonnegInt) :: PairList
  fun PairList.take_right(lst :: PairList, n :: NonnegInt) :: PairList
){

 Like @rhombus(PairList.take_left) and @rhombus(PairList.take_right), but for
 @tech{pair lists}. Producing the result list takes @math{O(N)} time,
 where @math{N} is the @rhombus(n) argument for taking from the left or
 the length of the pair list for taking from the right.

@examples(
  PairList[1, 2, 3, 4, 5].take_left(2)
  PairList[1, 2, 3, 4, 5].take_right(2)
  ~error:
    PairList[1].take_left(2)
)

}


@doc(
  fun PairList.drop_left(lst :: PairList, n :: NonnegInt) :: PairList
  fun PairList.drop_right(lst :: PairList, n :: NonnegInt) :: PairList
){

 Like @rhombus(PairList.drop_left) and @rhombus(PairList.drop_right), but for
 @tech{pair lists}. Producing the result list takes @math{O(N)} time,
 where @math{N} is the @rhombus(n) argument for dropping from the left or
 the length of the pair list for dropping from the right.

@examples(
  PairList[1, 2, 3, 4, 5].drop_left(2)
  PairList[1, 2, 3, 4, 5].drop_right(2)
  ~error:
    PairList[1].drop_left(2)
)

}


@doc(
  fun PairList.has_element(lst :: PairList, v :: Any) :: Boolean
){

 Returns @rhombus(#true) if @rhombus(lst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise. Searching the pair list
 takes @math{O(N)} time.

@examples(
  PairList[1, 2, 3].has_element(2)
  PairList[1, 2, 3].has_element(200)
)

}


@doc(
  fun PairList.remove(lst :: PairList, v :: Any) :: PairList
){

 Returns a @tech{pair list} like @rhombus(lst), but with the first element equal to
 @rhombus(v) (if any) removed.  Searching the pair list takes @math{O(N)} time.

@examples(
  [1, 2, 3, 2].remove(2)
)

}

@doc(
  fun PairList.map(lst :: PairList, f :: Function.of_arity(1))
    :: PairList,
  fun PairList.for_each(lst :: PairList, f :: Function.of_arity(1))
    :: Void,
){

 Like @rhombus(Function.map) and @rhombus(Function.for_each), but with a
 single @tech{pair list} of arguments first, with the function supplied second.

@examples(
  PairList.map(PairList[1, 2, 3], fun (x): x + 1)
  PairList[1, 2, 3].map(fun (x): x + 1)
  PairList[1, 2, 3].for_each(println)
)

}


@doc(
  fun PairList.sort(lst :: PairList,
                    is_less :: Function.of_arity(2) = math.less)
    :: PairList,
){

 Sorts @rhombus(lst) using @rhombus(is_less) to compare elements.
 Sorting takes @math{O(N log N)} time.

@examples(
  PairList.sort(PairList[1, 3, 2])
  PairList.sort(PairList[1, 3, 2], math.greater)
)

}


@doc(
  fun PairList.iota(n :: NonnegInt) :: PairList.of(NonnegInt)
){

 Returns a @tech{pair list} containing the integers 0 to @rhombus(n) (exclusive) in
 order.

@examples(
  PairList.iota(3)
  PairList.iota(0)
)

}


@doc(
  fun PairList.to_list(lst :: PairList) :: List
){

 Implements @rhombus(Listable, ~class) by returning a @tech{list}
 with the same elements as @rhombus(lst) in the same order. The conversion
 takes @math{O(N)} time.

}
