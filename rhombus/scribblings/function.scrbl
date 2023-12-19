#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@(def posn_eval = make_rhombus_eval())
@examples(
  ~eval: posn_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "functions_optional"){Optional and Keyword Arguments}

As we have seen, simple function definitions with @rhombus(fun) have the
function name, the parameters in parentheses, and the body after a
colon:

@examples(
  ~defn:
    fun avg(a, b):
      (a + b) / 2
  ~repl:
    avg(-1, 5)
)

The @rhombus(fun) form can be used in an expression position and
without a function name, in which case it behaves like λ and produces
an anonymous function value.

@examples(
  ~defn:
    def curried_add:
      fun (x):
        fun (y):
          x+y
  ~repl:
    curried_add(10)(20)
)

We've also seen that functions can have @rhombus(::) and @rhombus(:~)
annotations on both the parameters and the return value. In general, the
parameters can be bindings, which can express annotations and pattern
matching. All of that works with @rhombus(fun) in expression positions,
too.

@examples(
  ~defn:
    fun make_avg(a :: Number):
      fun (b :: Number) :: Number:
        (a + b) / 2
  ~repl:
    make_avg(5)(-2)
    ~error:
      make_avg(5)("hello")
)

A function argument can be made optional by using @rhombus(=) after the
argument’s pattern and providing a default-value expression after
@rhombus(=):

@examples(
  ~eval: posn_eval
  ~defn:
    fun scale(Posn(x, y), factor = 1):
      Posn(factor * x, factor * y)
  ~repl:
    scale(Posn(1, 2))
    scale(Posn(1, 2), 3)
)

When a function has multiple optional arguments, by-keyword arguments
are especially useful. A keyword argument is indicated by prefixing a
formal or actual argument with a shrubbery keyword, which is written
with a leading @litchar{~}, and then starting a block with @litchar{:}.

@examples(
  ~eval: posn_eval
  ~defn:
    fun transform(Posn(x, y),
                  ~scale: factor = 1,
                  ~dx: dx = 0,
                  ~dy: dy = 0):
      Posn(factor*x + dx, factor*y + dy)
  ~repl:
    transform(Posn(1, 2))
    transform(Posn(1, 2), ~dx: 7)
    transform(Posn(1, 2), ~dx: 7, ~scale: 2)
)

Since a keyword by itself is not allowed as an expression or pattern,
there is no possibility that a keyword will be inadvertently treated as
an actual argument or binding pattern by itself. The @rhombus(#')
prefix operator turns a keyword into an expression that produces the keyword, as in
@rhombus(#'~scale). The operator also works on identifiers, as in @rhombus(#'x),
to produce a symbol.

@margin_note{The keyword prefix and @rhombus(=) for default values are not
 binding operators. They are specific to the syntax of @rhombus(fun).}

If an argument name is the same as its keyword (just without the
@litchar{~}), then the @litchar{:} and argument name can be omitted.
That only works for an argument that would otherwise be just an
identifier and maybe a default value, because keywords don’t work as
variable names in binding patterns.

@examples(
  ~eval: posn_eval
  ~defn:
    fun transform(Posn(x, y),
                  ~scale: factor = 1,
                  ~dx = 0,
                  ~dy = 0):
      Posn(factor*x + dx, factor*y + dy)
  ~repl:
    transform(Posn(1, 2))
    transform(Posn(1, 2), ~dx: 7)
    transform(Posn(1, 2), ~dx: 7, ~scale: 2)
)

For functions that can take arbitrarily many arguments and
for functions with multiple cases, see
@seclink("more-arguments"){More Function Arguments}.


@(close_eval(posn_eval))
