#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def list_eval = macro.make_macro_eval())

@title(~tag: "annotation-list"){List Annotations}

As explained in @secref("list"), the @rhombus(List, ~annot) annotation
matches any list, while @rhombus(List.of, ~annot) matches a list whose
elements all satisfy another annotation.

@examples(
  ~eval: list_eval
  [1, "two", #'three] is_a List
  [1, "two", #'three] is_a List.of(String)
  ["1", "two", "three"] is_a List.of(String)
)

The @rhombus(matching, ~annot) form can create an annotation that
matches a list with a more specific shape, such as one that has an
integer as its first element and a string as is second element. A
@rhombus(List.tuple_of, ~annot) annotation is a kind of shorthand for a
@rhombus(matching, ~annot) annotation with a list pattern and
@rhombus(_, ~bind) wildcards. Using @brackets directly for an annotation
(i.e., @rhombus(#%brackets, ~annot)) is an even more compact form that
it is equivalent to using @rhombus(List.tuple_of, ~annot).

@examples(
  ~eval: list_eval
  [1, "two"] is_a matching([_ :: Int, _ :: String])
  [1, "two"] is_a List.tuple_of[Int, String]
  [1, "two"] is_a [Int, String]
  ["two", 1] is_a [Int, String]
)

A @rhombus(List.tuple_of, ~annot) or @brackets annotation can end with
@rhombus(..., ~bind) to indicate zero or more elements that match the
preceding annotation (but, unlike a list @rhombus(List, ~bind) pattern
in @rhombus(matching, ~annot), the @rhombus(..., ~bind) is allowed only at
the end of the annotation sequence). This means that
@rhombus([String, ...], ~annot) is another way to write
@rhombus(List.of(String), ~annot), for example.

@examples(
  ~eval: list_eval
  ["1", "two"] is_a List.of(String)
  ["1", "two"] is_a [String, ...]
  [1, "two", "three"] is_a [Int, String, ...]
)

A @rhombus(List.tuple_of, ~annot) or @brackets annotation supplies
element-specific static information, which is the sense in which it
corresponds to a tuple. A tuple-style list annotation can be an
alternative to @rhombus(values, ~annot), for example, that also works in
contexts where @rhombus(values, ~annot) is not allowed.

@examples(
  ~eval: list_eval
  use_static
  fun enumerated(s :: String, ...) :~ [[Int, String], ...]:
    for List (s in [s, ...],
              i in 0 ..):
      [i, s]
  def strs = enumerated("a", "bb", "cccc")
  strs
  def [[i0, str0], [i1, str1], [i2, str2]] = strs
  :
    str0.length() // static call to String.length
  str2.length()
)

A list construction using @rhombus(List) or @brackets propagates
element-specific, tuple-style static information to pattern matching,
but the @rhombus(List.get) or the @brackets indexing operation (i.e.,
@rhombus(#%index)) is not statically sensitive to a literal index.

@examples(
  ~eval: list_eval
  use_static
  class Posn(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn
  def ps && [p0, p1, p2] = [Posn(1, 2),
                            Posn3D(4, 5, 6),
                            Posn(7, 8)]
  p0.x
  p1.z
  ~error:
    p0.z
  :
    ps[1].x // because all elements are Posns
  ~error:
    :
      ps[1].z // not all elements are Posn3Ds
)

@(close_eval(list_eval))
