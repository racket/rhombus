#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "repetition"){Repetitions}

As we saw in @secref("list"), the list form supports repetition binding
via @rhombus(..., ~bind), and it can use a repetition via @rhombus(...)
when creating a new list:

@demo(
  def [x, ...] = [1, 2, 3]
  [x, ..., 100]
)

In either bindings or expressions, @rhombus(...) can be nested to create
and use repetitions of greater depth. For example, using
@rhombus(..., ~bind) within a term that is already before
@rhombus(..., ~bind) creates a binding of depth 2, which is a repetition
of repetitions.

@demo(
  def [[z, ...], ...] = [[1, 2, 3], [5, 6]]
  [[z, ..., 100], ...]
)

In this example, both @rhombus(..., ~bind)s are recognized directly by
the list binding form, but the list-construction expression follows a
more general rule. The outer @rhombus(...) (i.e., the last one)
allows any repetition of depth 1 before it, and @rhombus([z, ..., 100])
creates a repetition of depth 1 by using @rhombus(...) after
@rhombus(z), which is a repetition of depth 2.

When a repetition is followed by multiple @rhombus(...)s in a row, as
opposed to nested @rhombus(...)s, then the repetitions that would be
accessed by nesting are flattend into a single repetition. This
flattening has the effect of appending sequences.

@demo(
  def [[z, ...], ...] = [[1, 2, 3], [5, 6]]
  [z, ..., ...]
)

Some other expression-like forms serve as repetition forms when they are
used in a repetition position. For example, an operator that is defined
by @rhombus(operator) forms a reptition when it has repetition
arguments, so a negation term @rhombus(-x) creates a repetition of depth
1 when @rhombus(x) is a repetition of depth 1:

@demo(
  def [x, ...] = [1, 2, 3]
  [-x, ...]
)

When a repetition is built from multiple other repetitions, the
repetitions are used in parallel. For example, @rhombus(+) used on two
repetitions maps addition over the repetitions. The two repetitions must
have the same length.

@demo(
  def [x, ...] = [1, 2, 3]
  def [y, ...] = [4, 5, 6]
  [x+y, ...]
)

When repetitions of different depths are combined, the shallower
repetition is repeated for outer layers of the deeper repetition.

@demo(
  def [[z, ...], ...] = [[1, 2], [3, 4], [5, 6]]
  def [x, ...] = ["a", "b"]
  [[x +& z, ...], ...]
)

A literal value or a variable works as a repetition of depth 0. A
repetition of depth 0 is not useful in itself, but it's useful in
combination with a repetition of greater depth. For example, using
@rhombus(1) as a repetition lets us add it to every element of a
repetition.

@demo(
  def [x, ...] = [1, 2, 3]
  [x+1, ...]
  def five = 5
  [x+five, ...]
  ~error:
    [five, ...]
)

Analogous to lists, map and set constructions work as repetition forms.

@demo(
  def [x, ...] = [1, 2, 3]
  [{x}, ...]
  [{x: #true}, ...]
)

Function calls, the @rhombus(.) operator, array or map access via
@rhombus([]), and syntax templates all work as repetition forms, too,
given other repetitions to start with.

@demo(
  class Posn(x, y)
  fun mirror(Posn(x, y)) -: List.of(Posn):
    [Posn(x, y), Posn(y, x)]
  def [p, ...] = [Posn(1, 2), Posn(3, 4), Posn(5, 6)]
  ['z + $(mirror(p)[1].x)', ...]
)
