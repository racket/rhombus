#lang scribble/rhombus/manual
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
