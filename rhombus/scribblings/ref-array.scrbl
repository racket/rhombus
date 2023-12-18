#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))

@title{Arrays}

An array is @tech{indexable} using @brackets to access an array element
by position (in constant time) via @rhombus(#%index). An array also
works with the @rhombus(++) operator to append arrays. An array can be
used as @tech{sequence}, in which case it supplies its elements in
order.

An array is normally mutable, but immutable arrays can originate from
Racket. The @rhombus(Array, ~annot) annotation is satisfied by both
mutable and immutable arrays, while @rhombus(MutableArray, ~annot) and
@rhombus(ImmutableArray, ~annot) require one or the other.

@dispatch_table(
  "array",
  @rhombus(Array),
  [arr.length(), Array.length(arr)]
  [arr.copy(), Array.copy(arr)]
  [arr.copy_from(arg, ...), Array.copy_from(arr, arg, ...)]
)

@doc(
  annot.macro 'Array'
  annot.macro 'Array.now_of($annot)'
  annot.macro 'Array.later_of($annot)'
  annot.macro 'MutableArray'
  annot.macro 'ImmutableArray'
){

 The @rhombus(Array, ~annot) annotation (without @rhombus(now_of) or
 @rhombus(later_of)) matches any array.

 The @rhombus(Array.now_of, ~annot) form constructs a @tech{predicate
  annotation} that matches an array whose elements all currently satisfy
 @rhombus(annot), but it does not ensure in any way that future
 values installed into the array will satisfy @rhombus(annot). The given
 @rhombus(annot) must not be a converting annotation. Static
 information from @rhombus(annot) is not propagated to accesses of
 the array, since there's no gauarantee that the value will still satisfy
 the annotation.

 The @rhombus(Array.later_of, ~annot) form constructs a @tech{converter
  annotation} that immediately matches an array without checking
  that its elements currently satisfy @rhombus(annot). The conversion
 result of the annotation is a view of the original array, but one where
 @rhombus(annot) is checked against a value that would be returned by
 accessing an element of the array or a value to be installed into the
 array. (A different view of the array might changes an element to one that
 does not astisfy @rhombus(annot).) Static information from
 @rhombus(annot) is propagated to accesses of the array. Note that a
 converter @rhombus(annot) is applied for each access or update.

 @rhombus(MutableArray, ~annot) matches only mutable arrays, and
 @rhombus(ImmutableArray, ~annot) matches only immutable arrays (that may
 originate from Racket).

 Static information associated by @rhombus(Array, ~annot), etc., makes
 an expression acceptable as a sequence to @rhombus(for) in static mode.

@examples(
  ~repl:
    Array(1, 2, 3) :: Array.now_of(Number)
    ~error:
      Array(1, "b", 3) :: Array.now_of(Number)
  ~defn:
    def a :: Array.later_of(Number) = Array(1, "b", 3)
  ~repl:
    a[0]
    ~error:
      a[1]
    ~error:
      a[2] := "c"
)

}

@doc(
  fun Array(v :: Any, ...) :: MutableArray
){

 Constructs a mutable array containing given arguments.

@examples(
  def a: Array(1, 2, 3)
  a
  a[0]
  a[0] := 0
  a
)

}

@doc(
  ~nonterminal:
    repet_bind: def bind ~defn
  bind.macro 'Array($bind, ...)'
  bind.macro 'Array($bind, ..., $repet_bind #,(@litchar{,}) $ellipsis)'

  grammar ellipsis:
    #,(dots)
){

 Matches an array with as many elements as @rhombus(bind)s, where
 each element matches its corresponding @rhombus(bind), or at least
 as may elements as @rhombus(bind)s when a @rhombus(repet_bind) is
 provided.  When @rhombus(repet_bind) is provided, each additional element
 must match @rhombus(repet_bind).

 Elements are extracted from a matching array eagerly, so mutations of
 the array afterward do no change the matched values. When
 @rhombus(repet_bind) is provided, the extracted matching elements are
 combined into an internal list to implement the repetition.

@examples(
  def Array(1, x, y): Array(1, 2, 3)
  y
  def Array(1, z, ...): Array(1, 2, 3)
  [z, ...]
)

}

@doc(
  reducer.macro 'Array'
  reducer.macro 'Array ~length $expr'
){

 A @tech{reducer} used with @rhombus(for), accumulates each result of a
 @rhombus(for) body into a result array.

 When a @rhombus(~length) clause is provided, an array of the specified
 length is created and mutated by iterations of the @rhombus(for) body.
 Iterations more than the specified length will trigger an exception,
 while iterations fewer than the length will leave @rhombus(0) values in
 the array.

}

@doc(
  fun Array.make(length :: NonnegInt, val :: Any = 0)
    :: MutableArray
){

  Creates a fresh array with @rhombus(length) slots, where each slot
  is initialized to @rhombus(val).

@examples(
  Array.make(3, "x")
)

}

@doc(
  fun Array.length(arr :: Array) :: Int
){

 Returns the length of @rhombus(arr).

@examples(
  Array.make(3, "x").length()
)

}


@doc(
  fun Array.copy(arr :: Array) :: MutableArray
){

 Returns a fresh array string with the same initial content as
 @rhombus(arr).

}

@doc(
  fun Array.copy_from(dest_arr :: MutableArray,
                      dest_start :: NonnegInt,
                      src_arr :: Array,
                      src_start :: NonnegInt = 0,
                      src_end :: NonnegInt = Array.length(src_arr))
    :: Void
){

 Copies bytes from @rhombus(src_arr) at @rhombus(src_start) (inclusive) to
 @rhombus(src_end) (exclusive) into @rhombus(dest_arr) starting at
 @rhombus(dest_start). The length of @rhombus(dest_arr) must be at least
 @rhombus(dest_start + (src_end - src_start)).

}
