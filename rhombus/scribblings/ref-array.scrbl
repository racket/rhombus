#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Arrays}

An expression followed by a square-bracketed sequence of expressions
is parsed as an implicit use of the @rhombus(#%ref) form, which is
normally bound to implement an array reference or assignment, as well
as other operations.  An array can be used as @tech{sequence}, in which case
it supplies its elements in order.

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
  annot.macro 'Array.of($annot)'
  annot.macro 'MutableArray'
  annot.macro 'ImmutableArray'
){

 Matches any array in the form without @rhombus(of). The @rhombus(of)
 variant matches an array whose elements satisfy @rhombus(annotation).

 @rhombus(MutableArray, ~annot) matches only mutable arrays, and and
 @rhombus(ImmutableArray, ~annot) matches only immutable arrays (that may
 originate from Racket).

}

@doc(
  fun Array(v :: Any, ...) :: Array
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
  bind.macro 'Array($bind, ...)'
){

 Matches an array with as many elements as @rhombus(bind)s, where
 each element matches its corresponding @rhombus(bind).

@examples(
  def Array(1, x, y): Array(1, 2, 3)
  y
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
  fun Array.make(length :: Int, val = 0) :: Array
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
  fun Array.copy(arr :: Array) :: Array
){

 Returns a fresh array string with the same initial content as
 @rhombus(arr).

}

@doc(
  fun Array.copy_from(dest_arr :: Array,
                      dest_start :: NonnegInt,
                      src_arr :: Array,
                      src_start :: NonnegInt = 0,
                      src_end :: NonnegInt = Bytes.length(src_bstr)) :: Void
){

 Copies bytes from @rhombus(src_arr) at @rhombus(src_start) (inclusive) to
 @rhombus(src_end) (exclusive) into @rhombus(dest_arr) starting at
 @rhombus(dest_start). The length of @rhombus(dest_arr) must be at least 
 @rhombus(dest_start + (src_end - src_start)).

}
