#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def list_eval = make_rhombus_eval())

@demo(
  ~eval: list_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "list"){Lists}

A @brackets form as an expression creates a list:

@demo(
  ~eval: list_eval
  [1, 2, 3]
  [0, "apple", Posn(1, 2)]
)

Operations on lists include functions like @rhombus(ConsList.length), and
some of those operations can be applied using @rhombus(.) directly on an
expression that produces a list. The @rhombus(++) operator appends
lists.

@demo(
   ConsList.length(["a", "b", "c"])
   ["a", "b", "c"].length()
   ["a", "b"] ++ ["c", "d", "e"]
)

You can also use the @rhombus(ConsList) constructor, which takes any number of
arguments:

@demo(
  ConsList(1, 2, 3)
)

A list is a ``linked list,'' in the sense that getting the @math{n}th element
takes @math{O(n)} time, and adding to the front takes constant time. A
list is immutable.

@rhombus(ConsList, ~annot) works as an annotation with @rhombus(:~, ~bind) and
@rhombus(::, ~bind):

@demo(
  ~defn:
    fun
    | classify(_ :: ConsList): "list"
    | classify(_ :: Number): "number"
    | classify(_): "other"
  ~repl:
    classify([1])
    classify(1)
    classify("1")
)

As pattern, @brackets matches a list, and list elements
can be matched with specific subpatterns. The @rhombus(ConsList, ~bind) binding
operator works the same in bindings, too.

@demo(
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

@demo(
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
example, a @brackets or @rhombus(ConsList) expression (as
opposed to binding) supports @rhombus(...) in place of an element,
in which case the preceding element form is treated as a repetition
that supplies elements for the new list.


@demo(
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

@demo(
  ~eval: list_eval
  ~defn:
    def [groceries, ...] = ["apple", "banana", "milk"]
  ~repl:
    [groceries, ..., "cupcake"]
    [groceries, ..., groceries, ...]
)

Instead of using @rhombus(...) in @brackets
or @rhombus(ConsList) to bind or use a repetition, use @rhombus(&) to bind
or reference a plain list value whose elements are the rest of the list.

@demo(
  ~eval: list_eval
  ~defn:
    def [x, & others] = [groceries, ...]
  ~repl:
    others
    ["broccoli", & others ++ ["cupcake"], x]
    ~error: [others, ...]
    [groceries, ..., & ["pencil", "eraser"]]
)


When @brackets appears after an expression, then instead
of forming a list, it accesses an element of an @tech{indexable} value.
Lists are indexable using natural numbers starting with
@rhombus(0):

@demo(
  ~eval: list_eval
  ~repl:
    others[0]
    others[1]
)

Indexing with @brackets is sensitive to binding-based
static information in the same way as @rhombus(.). For example, a
function’s argument can use a binding pattern that indicates a list of
@rhombus(Posn)s, and then @rhombus(.) can be used after
@litchar{[}...@litchar{]} to efficiently access a field of a
@rhombus(Posn) instance:

@demo(
  ~eval: list_eval
  ~defn:
    fun nth_x([p :~ Posn, ...], n):
      [p, ...][n].x
  ~repl:
    nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1)
)

An equivalent way to write @rhombus(nth_x) is with the @rhombus(ConsList.of, ~annot)
annotation constructor. It expects an annotation that every element of
the list must satisfy:

@demo(
  ~eval: list_eval
  ~defn:
    fun nth_x(ps :~ ConsList.of(Posn), n):
      ps[n].x
)

The @rhombus(nth_x) function could have been written as follows, but
unlike the previous versions (where the relevant list existed as an
argument), this one creates a new intermediate list of @rhombus(x)
elements:

@demo(
  ~eval: list_eval
  ~defn:
    fun nth_x([Posn(x, _), ...], n):
      [x, ...][n]
  ~repl:
    nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1)
)

@close_eval(list_eval)
