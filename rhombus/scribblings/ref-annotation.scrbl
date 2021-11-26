#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Annotations}

@doc[
  operator (arg :: annotation) :: annotation
]{

 Checks that the value of @rhombus[arg] satisifies
 @rhombus[annotation], and returns the value if so.

@examples[
  [1, 2, 3] :: List
]

}

@doc[
  bind.rule '(binding :: annotation)
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

