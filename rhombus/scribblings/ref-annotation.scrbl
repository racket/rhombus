#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Annotations}

@doc[
  operator (expr :: annotation) :: annotation
]{

 Checks that the value of @rhombus[expr] satisifies
 @rhombus[annotation], and returns the value if so.

@examples[
  [1, 2, 3] :: List
]

}

@doc[
  bind.rule '($binding :: $annotation)
]{

 Binds the same as @rhombus[binding], but first checks that the value to
 be bound satisfies @rhombus[annotation].

@examples[
  val x :: List: [1, 2, 3]
]

}

@doc[
  annotation.macro 'Any
]{

  Matches any value.

}

@doc[
  operator (arg -: annotation) :: annotation
]{

 Associates static information to the overall expression the same as
 @rhombus[::], but performs no run-time check on the value of
 @rhombus[expr].

@examples[
  [1, 2, 3] -: List,
  "oops" -: List
]

}

@doc[
  bind.rule '($binding -: $annotation)
]{

 Associates static information to @rhombus[binding] the same as
 @rhombus[::, ~bind], but performs no run-time check.

@examples[
  val x -: List: [1, 2, 3],
  val x -: List: "oops"
]

}


@doc[
  expr.rule '($expr is_a $annotation)
]{

 Produces @rhombus[#true] if the value of @rhombus[expr]
 satisfies @rhombus[annotation], @rhombus[#false] otherwise.

@examples[
  [1, 2, 3] is_a List,
  "oops" is_a List
]

}
