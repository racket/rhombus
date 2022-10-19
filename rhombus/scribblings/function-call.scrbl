#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title{Function Calls}

Function calls have the function first and the parameters in parentheses.

@examples(
  ~label: #false,
  sqrt(25),
  expt(2, 3),
  List(5, 6, 7, 8),
  List.length([1, 2, 5]),
)

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

For passing a variable number of arguments into a function,
see @seclink("more-arguments"){More Function Arguments}.
