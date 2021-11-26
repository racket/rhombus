#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

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
  annotation.macro 'List
]{

 Matches any list.

}
