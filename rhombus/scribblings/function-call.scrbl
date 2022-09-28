#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val dots_expr: @rhombus(...))

@title{Function Calls}

@section{Simple function calls}

An expression of the form

@doc(
  expr.specsubform '$function_expr($argument_expr, ...)'
){
is a function call (when @rhombus(function_expr) is not a
"special" identifier such as a macro).

The value of each @rhombus(argument_expr) is passed as a
single positional argument to the function.

@examples(
  sqrt(25),
  expt(2, 3),
  List(5, 6, 7, 8),
  List.length([1, 2, 5]),
)}

@section{Rest arguments in function calls}

@doc(
  expr.specsubform '$function_expr($argument_expr, ..., & $list_expr)'
){
A function call with a @rhombus(&) rest argument.
The elements of the list value produced by
@rhombus(list_expr) are passed as additional arguments after
the single arguments.

@examples(
  sqrt(& [25]),
  expt(& [2, 3]),
  List(& [5, 6, 7, 8]),
  sqrt(25, & []),
  expt(2, & [3]),
  List(5, 6, & [7, 8]),
)}

@doc(
  expr.specsubform '$function_expr($argument_expr, ..., $repetition_expr, dots)',

  grammar dots:
    $$(dots_expr)
){
A function call with a @(dots_expr) repetition rest argument.

@examples(
  val [x, ...]: [2, 3],
  expt(x, ...),
  List(x, ...),
)
}

Only one positional rest argument is allowed in a function
call: either a @rhombus(&) rest argument, or a @(dots_expr)
repetition rest argument, (or neither) but not both.

@section{Keyword arguments in function calls}

Some functions accept keyword arguments.
For those, simple function calls can be of the form:

@doc(
  expr.specsubform '$fun_expr($arg, ...)',

  grammar arg:
    $arg_expr
    $keyword: $arg_expr,
){
@examples(
  fun pythag_hype(~a: a, ~b: b):
    sqrt(a*a + b*b),
  pythag_hype(~a: 3, ~b: 4),
  pythag_hype(~a: 5, ~b: 12),
  pythag_hype(~a: 8, ~b: 15),
  pythag_hype(~a: 7, ~b: 24),
)
}

@section{Keyword rest arguments in function calls}

As a variable number of positional arguments can be passed
to a function from a list with @rhombus(&) rest arguments,
a variable number of keyword arguments can be passed to a
function from a map with @rhombus(~&) keyword rest
arguments:

@doc(
  expr.specsubform '$fun_expr ($arg, ..., $rest, ...)',

  grammar arg:
    $arg_expr
    $keyword: $arg_expr,

  grammar rest:
    & $list_expr
    ~& $map_expr
    $repetition $$(@litchar{,}) $$(dots_expr)
){}

@examples(
  fun pythag_hype(~a: a, ~b: b):
    sqrt(a*a + b*b),
  pythag_hype(~& {keyword'~a': 3, keyword'~b': 4}),
  pythag_hype(~a: 5, ~& {keyword'~b': 12}),
  pythag_hype(~a: 8, ~b: 15, ~& {}),
  pythag_hype(~b: 24, ~& {keyword'~a': 7}),
)

