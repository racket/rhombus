#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "array"){Arrays}

The @rhombus(Array) constructor is similar to @rhombus(List), but it
creates an array, which has a fixed length at the time that it’s created
and offers constant-time access to any element of the array. Like a
list, and array is a map. Unlike a list, an array is mutable, so
@litchar{[}...@litchar{]} for indexing can be combined with @rhombus(:=)
for assignment.

@demo(
  ~defn:
    def buckets = Array(1, 2, 3, 4)
  ~repl:
    buckets[0]
    buckets[1] := 5
    buckets
)

@rhombus(Array) is also an annotation and a binding contructor,
analogous to @rhombus(List), and @rhombus(Array.of) is an annotation
constructor. The @rhombus(Array, ~bind) binding form does not support
@rhombus(..., ~bind) or @rhombus(&, ~bind), but the @rhombus(Array)
constructor supports @rhombus(..., ~bind) and @rhombus(&, ~bind).
