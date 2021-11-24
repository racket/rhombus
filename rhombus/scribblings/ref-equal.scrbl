#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Equality}

@doc[
  operator ((x :: Number) == (y :: Number)) :: Boolean
]{

 Reports whether @rhombus[x] and @rhombus[y] are numerically equal.

@examples[
  1 == 1,
  1 == 2,
  1.0 == 1
]

}

@doc[
  operator ((v1 :: Any) === (v1 :: Any)) :: Boolean
]{

 Reports whether @rhombus[v1] and @rhombus[v2] are equivalent.

@examples[
  "apple" === "apple",
  [1, 2, 3] === 1,
  [1, "apple", {"alice": 97}] === [1, "apple", {"alice": 97}],
  1 === 1.0
]

}