#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val dots_expr: @rhombus(...))

@title(~tag: "more-arguments"){More Function Arguments}

@section{Rest arguments with @rhombus(&) or @(dots_expr)}

One way to call a function with a variable number of
arguments is by passing them as a list into a @rhombus(&)
rest argument.

@examples(
  ~label: #false,
  sqrt(& [25]),
  expt(& [2, 3]),
  List(& [5, 6, 7, 8]),
  sqrt(25, & []),
  expt(2, & [3]),
  List(5, 6, & [7, 8]),
)

Another way to call a function with a variable number of
arguments is by passing them as a @(dots_expr) repetition
rest argument.

@examples(
  ~label: #false,
  val [x, ...]: [2, 3],
  expt(x, ...),
  List(x, ...),
)

Only one positional rest argument is allowed in a function
call: either a @rhombus(&) rest argument, or a @(dots_expr)
repetition rest argument, (or neither) but not both.

A function can be defined to take arbitrarily many arguments
by declaring a @rhombus(&) rest parameter to collect them
into a list.

@examples(
  ~label: #false,
  import: math,
  fun avg(a, & bs):
    (a + math.sum(bs)) / (1 + List.length(bs)),
  avg(1, 5),
  avg(7, 9),
  avg(-2, 4),
  avg(1, 2, 5),
  avg(-8, 5, 6, 7),
)

In the same way that @rhombus(..., ~bind) can be used in a list
binding pattern to create a repetition, @rhombus(..., ~bind) can be
used after the last argument in a function declaration to bind
repetitions. And just like @rhombus(...) can be used at the end of a
list expression to add the repetition's element to the end of the
list, @rhombus(...) can be used at the end of a function-call
expression to use the repetition's elements as the last arguments to
the function.

@examples(
  ~label: #false,
  fun
  | add(): 0
  | add(x): x
  | add(x, y, ...): x + add(y, ...),
  add(1, 2, 3),
  val [n, ...]: [10, 20, 30],
  add(1, n, ...),
)

@section{Keyword rest arguments with @rhombus(~&)}

As a variable number of positional arguments can be passed
to a function from a list with @rhombus(&) rest arguments,
a variable number of keyword arguments can be passed to a
function from a map with @rhombus(~&) keyword rest
arguments.

@examples(
  ~label: #false,
  fun pythag_hype(~a: a, ~b: b):
    sqrt(a*a + b*b),
  pythag_hype(~& {keyword'~a': 3, keyword'~b': 4}),
  pythag_hype(~a: 5, ~& {keyword'~b': 12}),
  pythag_hype(~a: 8, ~b: 15, ~& {}),
  pythag_hype(~b: 24, ~& {keyword'~a': 7}),
)

A function can be defined to take arbitrarily many keyword
arguments by declaring a @rhombus(~&) keyword rest parameter
to collect them into a map.

@examples(
  ~label: #false,
  fun anykws(~& m):
    m,
  anykws(~a: 1, ~b: 2),
)
