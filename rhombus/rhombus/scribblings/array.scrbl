#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "array"){Arrays}

The @rhombus(Array) constructor is similar to @rhombus(List), but it
creates an array, which has a fixed length at the time that it’s created
and offers constant-time access to any element of the array. Like a
list, an array is @tech{indexable}. Unlike a list, an array is mutable, so
@brackets for indexing can be combined with @rhombus(:=)
for assignment.

@examples(
  ~defn:
    def buckets = Array(1, 2, 3, 4)
  ~repl:
    buckets[0]
    buckets[1] := 5
    buckets
)

@rhombus(Array) is also an annotation and a binding contructor,
analogous to @rhombus(List), and @rhombus(Array.now_of, ~annot)
and @rhombus(Array.later_of, ~annot) are annotation
constructors. The @rhombus(Array, ~bind) binding form does not support
@rhombus(..., ~bind) or @rhombus(&, ~bind), but the @rhombus(Array)
constructor supports @rhombus(..., ~bind) and @rhombus(&, ~bind).

The @rhombus(MutableList) constructor and annotation corresponds to an
object that contains a list, where the object can be mutated to change
the list that it contains. Like an array, a mutable list supports
indexing and update via @brackets and @rhombus(:=). Unlike an array, a
mutable list supports operations that add or remove elements.

@examples(
  ~defn:
    def items = MutableList(1, 2, 3, 4)
  ~repl:
    items[0]
    items[1] := 5
    items
    items.insert(2, 2.5)
    items
    items.append([10, 20, 30])
    items
)
