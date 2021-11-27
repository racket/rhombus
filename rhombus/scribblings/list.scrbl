#lang scribble/rhombus/manual
@(import:
    "util.rhm": no_prefix
    "common.rhm": no_prefix)

@title[~tag: "list"]{Lists}

A @litchar{[}...@litchar{]} form as an expression creates a list:

@(rhombusblock:
    [1, 2, 3]                // prints [1, 2, 3]
    [0, "apple", Posn(1, 2)] // prints [0, "apple", Posn(1, 2)]
  )

You can also use the @rhombus[List] constructor, which takes any number of
arguments:

@(rhombusblock:
    List(1, 2, 3)  // prints [1, 2, 3]
  )

A list is a ``linked list,'' in the sense that getting the _n_th element
takes @math{O(n)} time, and adding to the front takes constant time. A
list is immutable.

@rhombus[List, ~ann] works as an annotation with @rhombus[-:, ~bind] and
@rhombus[::, ~bind]:

@(rhombusblock:
    fun
    | classify(_ :: List): "list"
    | classify(_ :: Number): "number"
    | classify(_): "other"

    classify([1])  // prints "list"
    classify(1)    // prints "number"
    classify("1")  // prints "other"
  )

As pattern, @litchar{[}...@litchar{]} matches a list, and list elements
can be matched with specific subpatterns. The @rhombus[List, ~bind] binding
operator works the same in bindings, too.

@(rhombusblock:
    fun three_sorted([a, b, c]):
      a <= b && b <= c

    three_sorted([1, 2, 3]) // prints #true
    three_sorted([1, 3, 2]) // prints #false
  )

The last element in a @litchar{[}...@litchar{]} binding pattern can be
@rhombus[...], which means zero or more repetitions of the preceding
pattern, and each variable bound by the preceding pattern is instead
bound to a list of matches.

@(rhombusblock:
    fun
    | got_milk([]): #false
    | got_milk([head, tail, ...]):
       head === "milk" || got_milk(tail)

    got_milk([])                             // prints #false
    got_milk(["apple", "milk", "banana"])    // prints #true
    got_milk(["apple", "coffee", "banana"])  // prints #false
  )

A use of @litchar{[}...@litchar{]} or @rhombus[List] for an expression
also supports @rhombus[...] in place of a last argument, in which case
the preceding argument is treated as a list that is the tail of the new
list.

@(rhombusblock:
    [1, 2, [3, 4], ...]  // prints [1, 2, 3, 4]

    fun
    | is_sorted([]): #true
    | is_sorted([head]): #true
    | is_sorted([head, next, tail, ...]):
       head <= next && is_sorted([next, tail, ...])

    is_sorted([1, 2, 3, 3, 5]) // prints #true
    is_sorted([1, 2, 9, 3, 5]) // prints #false
  )

When @litchar{[}...@litchar{]}appears after an expression, then instead
of forming a list, it accesses an element of an @tech{map} value.
Lists are maps that are indexed by natural numbers starting with
@rhombus[0]:

@(rhombusblock:
    val groceries: ["apple", "banana", "milk"]

    groceries[0] // prints "apple"
    groceries[2] // prints "milk"
  )

Indexing with @litchar{[}...@litchar{]} is sensitive to binding-based
static information in the same way as @rhombus[.] For example, a
function’s argument can use a binding pattern that indicates a list of
@rhombus[Posn]s, and then @rhombus[.] can be used after
@litchar{[}...@litchar{]} to efficiently access a field of a
@rhombus[Posn] instance:

@(rhombusblock:
    fun nth_x([ps -: Posn, ...], n):
      ps[n].x

    nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1) // prints 3
  )

An equivalent way to write @rhombus[nth_x] is with the @rhombus[List.of]
annotation constructor. It expects an annotation that every element of
the list must satisfy:

@(rhombusblock:
    fun nth_x(ps -: List.of(Posn), n):
      ps[n].x
  )

The @rhombus[nth_x] function could have been written as follows, but
unlike the previous versions, this one creates an intermediate list
@rhombus[xs]:

@(rhombusblock:
    fun nth_x([Posn(xs, _), ...], n):
      xs[n]

    nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1) // prints 3
  )