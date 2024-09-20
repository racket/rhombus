#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "function-shorthand"){Function Shorthand and Pipelines}

Functions are values, and they can be passed to other functions. For
example, the @rhombus(List.find) function (which can be called
equivalently as a @rhombus(find) method on a list) takes a predicate as
a function to determine which value to return, the @rhombus(List.map)
function (or @rhombus(map) method) takes a function to apply to each
element, and @rhombus(List.sort) (or @rhombus(sort) method), takes a
comparison function to determine when one element should be before
another.@margin_note{The @rhombus(List.map) method is about mapping a
 function over a list to produce a new list, and not about the
 @rhombus(Map, ~annot) datatype.}

@examples(
  List.find([1, 3, 2], fun (x): x > 2)
  [1, 3, 2].find(fun (x): x > 2)
  [1, 3, 2].map(fun (x): x + 1)
  [1, 3, 2].sort(fun (x, y): x > y)
)

We cannot pass @rhombus(>) directly to @rhombus(sort), because the
operator @rhombus(>) is not an expression by itself. Simple functions
that use operators, however, can be written with a shorthand of
@rhombus(_) within @parens:

@examples(
  List.find([1, 3, 2], (_ > 2))
  [1, 3, 2].find((_ > 2))
  [1, 3, 2].map((_ + 1))
  [1, 3, 2].sort((_ > _))
)

Each immediate @rhombus(_) in a @parens expression is turned into a
distinct argument for an anonymous function that replaces the
parenthesized expression. ``Immediate'' here means that the @rhombus(_)
is not nested more deeply within the @parens, such as being in a block
or under more parentheses. As the last example above illustrates,
@parens plus @rhombus(_) is a general way to turn on operator into a
function.

Another form of the @rhombus(_) shorthand is like a function call, but
where at least one argument in the call is exactly @rhombus(_) (not
combined with other terms). Similar to the conversion for @parens, each
separate @rhombus(_) becomes an argument to the function.

@examples(
  [1, 3, 2].map(math.max(_, 2))
  [[1, 3, 2], [6, 0, 5]].map(List.sort(_, fun (x, y): x > y))
  [[1, 3, 2], [6, 0, 5]].map(List.sort(_, (_ > _)))
)

The @rhombus(_) shorthand notations are designed to be relatively
unlikely to collide with an expression form, because @rhombus(_) by
itself as an expression is a syntax error. For example,
@rhombus(math.max(_, 2)) by itself is a syntax error. (Also,
@rhombus(_, ~bind) as a binding form matches without binding a name, so
@rhombus(_) is not easy to bind locally. The shorthand is limited,
however, and intended only for the simplest of cases. Fall back to
@rhombus(fun) at the first sign of trouble.

@examples(
  ~error:
    [1, 3, 2].map(2 * (_ + 1))
  [1, 3, 2].map(fun (x): 2 * (x + 1))
)

Function shorthands with @rhombus(_) can be particularly useful with the
@rhombus(|>) operator, which takes an argument and a function and calls
the function on the argument. The @rhombus(|>) operator is
left-associative with low precedence, so it can form @deftech{pipelines}
of function calls.

@examples(
  -1 |> math.abs
  -1 |> math.abs |> (_ * 2)
  [3, 1, 2]
    |> List.sort(_)
    |> ([0] ++ _ ++ [100])
    |> (_.map(to_string))
)
