#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val dots_expr: @rhombus(...))

@title{Function Calls}

@section{Simple function calls}

Function calls have the function first and the parameters in parentheses.

@examples(
  ~label: #false,
  sqrt(25),
  expt(2, 3),
  List(5, 6, 7, 8),
  List.length([1, 2, 5]),
)

@section{Rest arguments in function calls}

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

@section{Keyword arguments in function calls}

Some functions accept keyword arguments.
Keyword arguments are passed with the keyword first, then
the argument expression after a colon.

@examples(
  ~label: #false,
  fun pythag_hype(~a: a, ~b: b):
    sqrt(a*a + b*b),
  pythag_hype(~a: 3, ~b: 4),
  pythag_hype(~a: 5, ~b: 12),
  pythag_hype(~a: 8, ~b: 15),
  pythag_hype(~a: 7, ~b: 24),
)

@section{Keyword rest arguments in function calls}

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

