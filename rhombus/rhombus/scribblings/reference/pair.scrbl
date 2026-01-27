#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title(~tag: "Pairs"){Pairs and Pair Lists}

A @deftech{pair} combines two values: a ``first'' value and a ``rest''
value. A @deftech{pair list} is a @tech{listable} value that is
constructed with pairs and the empty pair list; every non-empty pair
list is a pair, while a pair represents a pair list only if its
``rest'' is a pair list.

@margin_note_block{A pair list is an immutable linked list. A Rhombus
 pair list is also a Racket list.}

A pair list is @tech{indexable} using @brackets to access a list
element by position---in @math{O(N)} time---via @rhombus(#%index). A
pair list also works with the @rhombus(++) operator to append pair
lists. A pair list supports @tech{membership tests} using the
@rhombus(in) operator. A pair list can be used as @tech{stream} or @tech{sequence}, in
which case it supplies its elements in order.

The empty pair-list value is unique and @rhombus(===) to itself. Two
pairs are equal as long as the ``first'' and ``rest'' values are
pairwire equal by @rhombus(==). Consequently, two pair lists are equal
by @rhombus(==) if they have the same length and their elements are
pairwise equal by @rhombus(==).

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
  fun Pair(fst :: Any, rst :: Any)
    :: Pair.of(Any.like(fst), Any.like(rst))
){

 Constructs a pair containing @rhombus(fst) and @rhombus(rst).

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
  fun Pair.cons(fst :: Any, rst :: Any)
    :: Pair.of(Any.like(fst), Any.like(rst))
  bind.macro 'Pair.cons($fst_bind, $rst_bind)'
){

 Aliases for @rhombus(Pair) in expression and binding positions.

 Note that the difference between @rhombus(Pair.cons) and
 @rhombus(PairList.cons) is that @rhombus(PairList.cons) requires a pair list as its
 second argument, which means that it always forms a @tech{pair list}. In
 contrast, @rhombus(Pair.cons) allows any value as its second
 argument---but it creates a pair list if that argument is a pair list.

@examples(
  Pair.cons(1, 2)
  Pair.cons(1, PairList[2, 3])
)

}

@doc(
  property (pr :: Pair).first
    :: Any.like_first(pr)
  property (pr :: Pair).rest
    :: Any.like_rest(pr)
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
  annot.macro 'PairList.tuple_of[$annot, ..., $maybe_ellipsis]'
  grammar maybe_ellipsis
  | #,(epsilon)
  | $ellipsis
  grammar ellipsis
  | #,(dots)

){

 The @rhombus(PairList, ~annot) annotation by itself matches any
 @tech{pair list}. The @rhombus(PairList.of, ~annot) variant matches a
 pair list whose elements all satisfy @rhombus(annot).

 The @rhombus(PairList.tuple_of, ~annot) annotation is analogous to the
 @rhombus(List.tuple_of, ~annot) annotation, but for pair lists.

 Static information associated by @rhombus(PairList, ~annot) or
 @rhombus(PairList.of, ~annot) makes an expression acceptable as a sequence
 to @rhombus(for) in static mode.

}

@doc(
  ~nonterminal:
    listable_expr: block expr
  fun PairList(v :: Any, ...)
    :: PairList.of(Any.like(v))
  expr.macro 'PairList[$expr_or_splice, ...]'
  repet.macro 'PairList[$repet_or_splice, ...]'

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

 Constructs a @tech{pair list} of the given @rhombus(v)s values or results of the
 @rhombus(expr_or_splice)s. A @rhombus(&) or @dots_expr form
 can appear within @brackets to splice a @tech{repetition} or existing @tech{listable}
 value into the constructed pair list, the same as in a function call (see
 @rhombus(#%call)). Pair-list constructions can also serve as
 repetitions, where @rhombus(repet_or_splice) is like
 @rhombus(expr_or_splice), but with repetitions in place of expressions.

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
    pair_list_repet_bind: def bind ~defn
    repet_bind: def bind ~defn
  bind.macro 'PairList($bind_or_splice, ...)'
  bind.macro 'PairList[$bind_or_splice, ...]'
  grammar bind_or_splice
  | $bind
  | $splice
  grammar splice
  | $repet_bind #,(@litchar{,}) $ellipsis
  | #,(@rhombus(&, ~bind)) $pair_list_bind
  | #,(@rhombus(&, ~bind)) $pair_list_repet_bind #,(@litchar{,}) $ellipsis
  grammar ellipsis
  | #,(dots)
  | #,(dots) ~nonempty
  | #,(dots) ~once
){

 Matches a @tech{pair list} with @rhombus(bind_or_splice)s in the same
 way that @rhombus(List, ~bind) matches lists.

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

 Unlike a @rhombus(List, ~bind), a @rhombus(splice) that does not impose
 a predicate or conversion on a matching value is constant-time only when
 the @rhombus(splice) is at the end of the pattern. Matching @math{N}
 elements to a splice in the middle of a pair list takes at least
 @math{O(N)} time. Along those lines, when multiple @rhombus(splice)s are
 among the @rhombus(bind_or_splice)s, a splice using
 @rhombus(#,(@rhombus(&, ~bind)) pair_list_bind) requires @math{O(N)}
 time to produce each @math{N}-length candidate match to
 @rhombus(pair_list_bind).

}

@doc(
  annot.macro 'NonemptyPairList':
    ~method_fallback: PairList
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

 A @tech(~doc: guide_doc){reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result @tech{pair list}.

}

@doc(
  fun PairList.cons(elem :: Any, lst :: PairList)
    :: PairList.of(Any.like(elem) || Any.like_element(lst))
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
  method (lst :: PairList).get(n :: Nat)
    :: Any.like_element(lst)
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
  property PairList.first(lst :: NonemptyPairList)
    :: Any.like_element(lst)
){

 Returns the first element of @rhombus(lst).
 Accessing the first element takes @math{O(1)} time.

@examples(
  PairList.first(PairList["a", "b", "c"])
  PairList["a", "b", "c"].first
)

}

@doc(
  property PairList.last(lst :: NonemptyPairList)
    :: Any.like_element(lst)
){

 Returns the last element of @rhombus(lst).
 Accessing the last element takes @math{O(N)} time.

@examples(
  PairList.last(PairList["a", "b", "c"])
  PairList["a", "b", "c"].last
)

}

@doc(
  property PairList.rest(lst :: NonemptyPairList)
    :: PairList.of(Any.like_element(lst))
){

 Returns a @tech{pair list} like @rhombus(lst), but without its first element.
 For a given pair list, @rhombus(List.rest) always produces the same value
 in the sense of @rhombus(===), and accessing that values takes @math{O(1)} time.

@examples(
  PairList.rest(PairList["a", "b", "c"])
  PairList["a", "b", "c"].rest
)

}


@doc(
  method (lst :: PairList).length() :: Nat
){

 Returns the number of items in @rhombus(lst).
 Computing this length takes @math{O(N)} time.

@examples(
  PairList.length(PairList[1, 4, 8])
  PairList.length(PairList[])
  PairList[1, 4, 8].length()
  PairList[].length()
)

}


@doc(
  method (lst :: PairList).reverse()
    :: PairList.of(Any.like_element(lst))
){

 Returns a @tech{pair list} with the same items as @rhombus(lst), but in reversed
 order. Reversing a list takes @math{O(N)} time.

@examples(
  PairList.reverse(PairList[1, 4, 8])
  PairList[1, 4, 8].reverse()
)

}


@doc(
  method (lst :: PairList).append(lst2 :: PairList, ...)
    :: PairList.of(Any.like_element(lst) || Any.like_element(lst2))
  fun PairList.append(lst :: PairList, ...)
    :: PairList.of(Any.like_element(lst))
){

 Appends the @rhombus(lst)s in order. See also @rhombus(++).
 Appending takes @math{O(N)} time.

@examples(
  PairList[1, 2, 3].append(PairList[4, 5], PairList[6])
  PairList.append(PairList[1, 2, 3], PairList[4, 5], PairList[6])
  PairList.append()
)

}


@doc(
  method (lst :: PairList).take(n :: Nat)
    :: PairList.of(Any.like_element(lst))
  method (lst :: PairList).take_last(n :: Nat)
    :: PairList.of(Any.like_element(lst))
){

 Like @rhombus(List.take) and @rhombus(List.take_last), but for
 @tech{pair lists}. Producing the result list takes @math{O(N)} time,
 where @math{N} is the @rhombus(n) argument for taking from the left or
 the length of the pair list for taking from the right.

@examples(
  PairList[1, 2, 3, 4, 5].take(2)
  PairList[1, 2, 3, 4, 5].take_last(2)
  ~error:
    PairList[1].take(2)
)

}


@doc(
  method (lst :: PairList).drop(n :: Nat)
    :: PairList.of(Any.like_element(lst))
  method (lst :: PairList).drop_last(n :: Nat)
    :: PairList.of(Any.like_element(lst))
){

 Like @rhombus(List.drop) and @rhombus(List.drop_last), but for
 @tech{pair lists}. Producing the result list takes @math{O(N)} time,
 where @math{N} is the @rhombus(n) argument for dropping from the left or
 the length of the pair list for dropping from the right.

@examples(
  PairList[1, 2, 3, 4, 5].drop(2)
  PairList[1, 2, 3, 4, 5].drop_last(2)
  ~error:
    PairList[1].drop(2)
)

}


@doc(
  method (lst :: PairList).contains(v :: Any,
                                    eqls :: Function.of_arity(2) = (_ == _))
    :: Boolean
){

 Returns @rhombus(#true) if @rhombus(lst) has an element equal to
 @rhombus(v), @rhombus(#false) otherwise, where @rhombus(eqls) determines
 equality. Searching the list takes @math{O(N)} time (multiplied by the
 cost of @rhombus(eqls)) to find an element at position @math{N}. See
 also @rhombus(in).

@examples(
  PairList[1, 2, 3].contains(2)
  PairList[1, 2, 3].contains(200)
  PairList[10, 20, 30].contains(20.0, (_ .= _))
  2 in PairList[1, 2, 3]
)

}


@doc(
  method (lst :: PairList).index(v :: Any,
                                 eqls :: Function.of_arity(2) = (_ == _))
    :: maybe(Int)
){

 Like @rhombus(PairList.contains), but instead of returning
 @rhombus(#true), returns the index of a found element.

@examples(
  PairList["a", "b", "c"].index("b")
  PairList["a", "b", "c"].index("d")
  PairList[10, 20, 30].index(20.0, (_ .= _))
)

}


@doc(
  method (lst :: PairList).find(pred :: Function.of_arity(1))
    :: Any.like_element(lst)
  method (lst :: PairList).find_index(pred :: Function.of_arity(1))
    :: maybe(Nat)
){

 Like @rhombus(List.find) and @rhombus(List.index), but for @tech{pair
  lists}. Searching the list takes @math{O(N)} time (multiplied by the
 cost of @rhombus(pred)) to find an element at position @math{N}.

@examples(
  PairList[1, 2, 3].find((_ mod 2 .= 0))
  PairList[1, 2, 3].find((_ mod 10 .= 9))
  PairList[1, 2, 3].find_index((_ mod 2 .= 0))
  PairList[1, 2, 3].find_index((_ mod 10 .= 9))
)

}


@doc(
  method (lst :: PairList).remove(v :: Any)
    :: PairList.of(Any.like_element(lst))
){

 Returns a @tech{pair list} like @rhombus(lst), but with the first element equal to
 @rhombus(v) (if any) removed.  Searching the pair list takes @math{O(N)} time.

@examples(
  [1, 2, 3, 2].remove(2)
)

}

@doc(
  method (lst :: PairList).map(f :: Function.of_arity(1))
    :: PairList
  method (lst :: PairList).for_each(f :: Function.of_arity(1))
    :: Void
){

 Like @rhombus(Function.map) and @rhombus(Function.for_each), but with a
 single @tech{pair list} of arguments first, with the function supplied second.

@examples(
  PairList.map(PairList[1, 2, 3], (_ + 1))
  PairList[1, 2, 3].map((_ + 1))
  PairList[1, 2, 3].for_each(println)
)

}


@doc(
  method (lst :: PairList).filter(
    ~keep: keep_pred :: Function.of_arity(1)
             = fun (_): #true,
    ~skip: skip_pred :: Function.of_arity(1)
             = fun (_): #false,
  ) :: PairList.of(Any.like_element(lst))
  method (lst :: PairList).partition(pred :: Function.of_arity(1))
    :: values(PairList.of(Any.like_element(lst)),
              PairList.of(Any.like_element(lst)))
){

 List @rhombus(List.filter) and @rhombus(List.partition), but for
 @tech{pair lists}.

@examples(
  PairList[1, -1, -2, 2].filter(~keep: (_ > 0))
  PairList[1, -1, -2, 2].filter(~skip: (_ > 0))
  PairList[1, -1, -2, 2].filter(~keep: (_ != -2), ~skip: (_ > 0))
  PairList[1, -1, -2, 2].partition((_ > 0))
)

}


@doc(
  method (lst :: PairList).sort(is_less :: Function.of_arity(2) = (_ < _))
    :: PairList.of(Any.like_element(lst))
){

 Sorts @rhombus(lst) using @rhombus(is_less) to compare elements.
 Sorting takes @math{O(N log N)} time.

@examples(
  PairList.sort(PairList[1, 3, 2])
  PairList.sort(PairList[1, 3, 2], (_ > _))
)

}


@doc(
  fun PairList.iota(n :: Nat) :: PairList.of(Nat)
){

 Returns a @tech{pair list} containing the integers 0 to @rhombus(n) (exclusive) in
 order.

@examples(
  PairList.iota(3)
  PairList.iota(0)
)

}


@doc(
  method (lst :: PairList).to_list()
    :: List.of(Any.like_element(lst))
){

 Implements @rhombus(Listable, ~class) by returning a @tech{list}
 with the same elements as @rhombus(lst) in the same order. The conversion
 takes @math{O(N)} time.

@examples(
  PairList[1, 2, 3].to_list()
  [& PairList[1, 2, 3]]
)

}


@doc(
  method (lst :: PairList).to_sequence()
    :: Sequence.expect_of(Any.like_element(lst))
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of @rhombus(lst)'s elements in order.

@examples(
  :
    for List (v in PairList[1, 2, 3]): // optimizing
      v
  :
    for List (v in PairList[1, 2, 3].to_sequence()): // non-optimizing
      v
)

}
