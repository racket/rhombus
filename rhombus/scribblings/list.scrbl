#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val list_eval: make_rhombus_eval())

@examples(
  ~eval: list_eval,
  ~hidden: #true,
  class Posn(x, y)
)

@title(~tag: "list"){Lists}

A @litchar{[}...@litchar{]} form as an expression creates a list:

@(demo:
    ~eval: list_eval
    [1, 2, 3]
    [0, "apple", Posn(1, 2)]
  )

Operations on lists include functions like @rhombus(List.length), and
some of those operations can be applied using @rhombus(.) directly on an
expression that produces a list. The @rhombus(++) operator appends
lists.

@(demo:
   List.length(["a", "b", "c"])
   ["a", "b", "c"].length()
   ["a", "b"] ++ ["c", "d", "e"]
  )

You can also use the @rhombus(List) constructor, which takes any number of
arguments:

@(demo:
    List(1, 2, 3)
  )

A list is a ``linked list,'' in the sense that getting the @math{n}th element
takes @math{O(n)} time, and adding to the front takes constant time. A
list is immutable.

@rhombus(List, ~ann) works as an annotation with @rhombus(-:, ~bind) and
@rhombus(::, ~bind):

@(demo:
    ~defn:
      fun
      | classify(_ :: List): "list"
      | classify(_ :: Number): "number"
      | classify(_): "other"
    ~repl:
      classify([1])
      classify(1)
      classify("1")
  )

As pattern, @litchar{[}...@litchar{]} matches a list, and list elements
can be matched with specific subpatterns. The @rhombus(List, ~bind) binding
operator works the same in bindings, too.

@(demo:
    ~defn:
      fun are_three_sorted([a, b, c]):
        a <= b && b <= c
    ~repl:
      are_three_sorted([1, 2, 3])
      are_three_sorted([1, 3, 2])
  )

The last element in a @litchar{[}...@litchar{]} binding pattern can be
@rhombus(..., ~bind), which means zero or more repetitions of the preceding
pattern.

@(demo:
    ~defn:
      fun
      | starts_milk([]): #false
      | starts_milk([head, tail, ...]): head == "milk"
    ~repl:
      starts_milk([])
      starts_milk(["milk", "apple", "banana"])
      starts_milk(["apple", "coffee", "banana"])
  )

Each variable in a pattern preceding @rhombus(..., ~bind) is bound as a
@tech{repetition}, which cannot be used like a plain variable.
Instead, a repetition variable must be used in an expression form that
supports using repetitions, typically with before @rhombus(...). For
example, a @litchar{[}...@litchar{]} or @rhombus(List) expression (as
opposed to binding) supports @rhombus(...) in place of an element,
in which case the preceding element form is treated as a repetition
that supplies elements for the new list.


@(demo:
    ~defn:
      fun
      | got_milk([]): #false
      | got_milk([head, tail, ...]):
         head == "milk" || got_milk([tail, ...])
    ~repl:
      got_milk([])
      got_milk(["apple", "milk", "banana"])
      got_milk(["apple", "coffee", "banana"])
  )

While @rhombus(..., ~bind) can only be used at the end of a list in a
binding, @rhombus(...) can be used anywhere in an expression, and it can
be used multiple times.

@(demo:
    ~eval: list_eval
    ~defn:
      val [groceries, ...]: ["apple", "banana", "milk"]
    ~repl:
      [groceries, ..., "cupcake"]
      [groceries, ..., groceries, ...]
  )

Instead of using @rhombus(...) in @litchar{[}...@litchar{]}
or @rhombus(List) to bind or use a repetition, use @rhombus(&) to bind
or reference a plain list value whose elements are the rest of the list.

@(demo:
    ~eval: list_eval
    ~defn:
      val [x, & others]: [groceries, ...]
    ~repl:
      others
      ["broccoli", & others ++ ["cupcake"], x]
      ~error: [others, ...]
      [groceries, ..., & ["pencil", "eraser"]]
  )


When @litchar{[}...@litchar{]} appears after an expression, then instead
of forming a list, it accesses an element of an @tech{map} value.
Lists are maps that are indexed by natural numbers starting with
@rhombus(0):

@(demo:
    ~eval: list_eval
    ~repl:
      others[0]
      others[1]
  )

Indexing with @litchar{[}...@litchar{]} is sensitive to binding-based
static information in the same way as @rhombus(.). For example, a
function’s argument can use a binding pattern that indicates a list of
@rhombus(Posn)s, and then @rhombus(.) can be used after
@litchar{[}...@litchar{]} to efficiently access a field of a
@rhombus(Posn) instance:

@(demo:
    ~eval: list_eval
    ~defn:
      fun nth_x([p -: Posn, ...], n):
        [p, ...][n].x
    ~repl:
      nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1)
  )

An equivalent way to write @rhombus(nth_x) is with the @rhombus(List.of, ~ann)
annotation constructor. It expects an annotation that every element of
the list must satisfy:

@(demo:
    ~eval: list_eval
    ~defn:
      fun nth_x(ps -: List.of(Posn), n):
        ps[n].x
  )

The @rhombus(nth_x) function could have been written as follows, but
unlike the previous versions (where the relevant list existed as an
argument), this one creates a new intermediate list of @rhombus(x)
elements:

@(demo:
    ~eval: list_eval
    ~defn:
      fun nth_x([Posn(x, _), ...], n):
        [x, ...][n]
    ~repl:
      nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1)
  )

@close_eval(list_eval)
