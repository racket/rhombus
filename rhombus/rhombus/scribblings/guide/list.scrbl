#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def list_eval = make_rhombus_eval())
@examples(
  ~eval: list_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "list"){Lists}

A @brackets form as an expression creates a list:

@examples(
  ~eval: list_eval
  [1, 2, 3]
  [0, "apple", Posn(1, 2)]
)

Operations on lists include functions like @rhombus(List.length), and
some of those operations can be applied using @rhombus(.) directly on an
expression that produces a list. The @rhombus(++) operator appends
lists.

@examples(
   List.length(["a", "b", "c"])
   ["a", "b", "c"].length()
   ["a", "b"] ++ ["c", "d", "e"]
)

You can also use the @rhombus(List) constructor, which takes any number of
arguments:

@examples(
  List(1, 2, 3)
)

@rhombus(List, ~annot) works as an annotation with @rhombus(:~, ~bind) and
@rhombus(::, ~bind):

@examples(
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

As pattern, @brackets matches a list, and list elements
can be matched with specific subpatterns. The @rhombus(List, ~bind) binding
operator works the same in bindings, too.

@examples(
  ~defn:
    fun are_three_sorted([a, b, c]):
      a <= b && b <= c
  ~repl:
    are_three_sorted([1, 2, 3])
    are_three_sorted([1, 3, 2])
)

The last element in a @brackets binding pattern can be
@rhombus(..., ~bind), which means zero or more repetitions of the preceding
pattern.

@examples(
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
@tech(~doc: ref_doc){repetition}, which cannot be used like a plain variable.
Instead, a repetition variable must be used in an expression form that
supports using repetitions, typically before @rhombus(...). For
example, a @brackets or @rhombus(List) expression (as
opposed to binding) supports @rhombus(...) in place of an element,
in which case the preceding element form is treated as a repetition
that supplies elements for the new list.


@examples(
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

A @rhombus(...) can be used anywhere in a binding or expression, and it can
be used multiple times.

@examples(
  ~eval: list_eval
  ~defn:
    def [grocery, ...] = ["apple", "banana", "milk"]
  ~repl:
    [grocery, ..., "cupcake"]
    [grocery, ..., grocery, ...]
)

Instead of using @rhombus(...) in @brackets
or @rhombus(List) to bind or use a repetition, use @rhombus(&) to bind
or reference a plain list value whose elements are the rest of the list.

@examples(
  ~eval: list_eval
  ~defn:
    def [x, & others] = [grocery, ...]
  ~repl:
    others
    ["broccoli", & others ++ ["cupcake"], x]
    ~error:
      [others, ...]
    [grocery, ..., & ["pencil", "eraser"]]
)

When @brackets appears after an expression, then instead
of forming a list, it accesses an element of an @tech(~doc: ref_doc){indexable} value.
Lists are indexable using natural numbers starting with
@rhombus(0):

@examples(
  ~eval: list_eval
  ~repl:
    others[0]
    others[1]
)

Indexing with @brackets is sensitive to binding-based
static information in the same way as @rhombus(.). For example, a
function's argument can use a binding pattern that indicates a list of
@rhombus(Posn)s, and then @rhombus(.) can be used after
@litchar{[}...@litchar{]} to efficiently access a field of a
@rhombus(Posn) instance:

@examples(
  ~eval: list_eval
  ~defn:
    fun nth_x([p :~ Posn, ...], n):
      [p, ...][n].x
  ~repl:
    nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1)
)

An equivalent way to write @rhombus(nth_x) is with the @rhombus(List.of, ~annot)
annotation constructor. It expects an annotation that every element of
the list must satisfy:

@examples(
  ~eval: list_eval
  ~defn:
    fun nth_x(ps :~ List.of(Posn), n):
      ps[n].x
)

The @rhombus(nth_x) function could have been written as follows, but
unlike the previous versions (where the relevant list existed as an
argument), this one creates a new intermediate list of @rhombus(x)
elements:

@examples(
  ~eval: list_eval
  ~defn:
    fun nth_x([Posn(x, _), ...], n):
      [x, ...][n]
  ~repl:
    nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1)
)

Many operations on a list with @math{N} elements take @math{O(log N)}
time, including getting an element of a list with @brackets, appending
lists with @rhombus(++), adding to the front or end of a list with
@rhombus(List.cons) or @rhombus(List.add), inserting into the middle of
a list with @rhombus(List.insert), deleting a list element with
@rhombus(List.delete), or dropping or taking a list prefix or suffix
with functions like @rhombus(List.drop_left). Internally, lists are
implemented as relaxed radix balanced (RRB) trees.

As an alternative to @rhombus(List), @rhombus(PairList) constructs a
@tech(~doc: ref_doc){pair list}, which is implemented as a singly linked list. As the
name suggests, a pair list uses a @tech(~doc: ref_doc){pair} to add each element to the
front of an existing list. The @rhombus(PairList.cons) operation
performs that addition, and it takes @math{O(1)} time, as do its
@rhombus(PairList.first) and @rhombus(PairList.rest) operations to access
the initial pair's components. Operations like appending or accessing a
random element take @math{O(N)} time. When @rhombus(PairList) prefixes
@brackets for constructing or matching a list, then @brackets constructs
or matches a pair list, instead of a list.

@examples(
  ~eval: list_eval
  ~defn:
    def autos = PairList["beetle", "jeep", "miata"]
  ~repl:
    Pair.cons("cadillac", autos)
    autos is_a List
    autos is_a Listable
)

The @rhombus(Listable, ~class) annotation is satisfied by a list, pair
list, or another data structure that implements a conversion to list
form. Listable values are generally allowed for explicit splicing
operations, such as using @rhombus(&) to build a list. One consequence
is that @rhombus(&) within a constructor can be used to convert among
listable representations, potentially while also adding additional
elements.

@examples(
  ~eval: list_eval
  autos
  [& autos]
  PairList["cadillac", & [& autos, "corvette"]]
  PairList["cadillac", & [& autos, "corvette"]]
)

@(close_eval(list_eval))
