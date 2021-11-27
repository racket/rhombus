#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@(val dots: @rhombus[..., ~bind])

@title{Lists}

@doc[
  fun List(v :: Any, ...) :: List
]{

 Constructs a list of the given arguments, equivalent to using
 @rhombus[[v, ...]].

@examples[
  List(1, 2, 3)
]

}

@doc[
  bind.macro '(List($binding, ...)),
  bind.macro '(List($binding, ..., $dots)),
  grammar dots:
    $$(dots)
]{

 Matches a list with as many elements as @rhombus[binding]s, or if
 @rhombus[dots] is included, at least as many elements as
 @rhombus[binding]s before the last one, and then them last one is
 matched against the rest of the list.

@examples[
  val List(1, x, y): [1, 2, 3],
  y,
  val List(1, xs, ...): [1, 2, 3],
  xs
]

}

@doc[
  annotation.macro 'List
]{

 Matches any list.

}

@doc[
  bind.macro '(...)
]{

 Used within binding patterns such as @rhombus[List, ~bind] or
 @rhombus[', ~bind] to indicate repetation.
}

