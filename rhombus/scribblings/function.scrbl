#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title{Functions}

@section{Simple function definitions}

@doc(
  defn.specsubform 'fun $id($param_id, ...):
                      $body
                      ...'
){
Defines @rhombus(id) as a function that takes the
@rhombus(param_id)s as inputs and produces the result of the
@rhombus(body) block as output.

@examples(
  fun avg(a, b):
    (a + b) / 2,
  avg(1, 5),
  avg(7, 9),
  avg(-2, 4),
)
}

Functions can be defined in namespaces as well, with an
@rhombus(id_path) allowed in place of a simple @rhombus(id).

Functions can also have @rhombus(::) and @rhombus(-:)
annotations, on both the parameters and the return value.
In general, the parameters can be bindings, which can
express annotations and pattern matching.

@doc(
  defn.specsubform 'fun $id_path($param_binding, ...) $maybe_result_annotation:
                      $body
                      ...',

  grammar maybe_result_annotation:
    :: $annotation
    -: $annotation
    $$("ϵ"),
){
Defines @rhombus(id_path) as a function that applies
bindings to its input values, which includes checking any
parameter @rhombus(::) annotations that exist on its inputs,
as well as checking the result @rhombus(::) annotation if it
exists on its output.

@examples(
  fun avg(a :: Number, b :: Number) :: Number:
    (a + b) / 2,
  avg(1, 5),
  avg(7, 9),
  avg(-2, 4),
  ~error avg("not a number", "doesn't pass"),
)
}

@section{Function Expressions}

The @rhombus(fun) form works in an expression position as λ. Just like
@tt{function} in JavaScript, the expression variant omits a function
name.

@(rhombusblock:
    val curried_add: fun (x):
                       fun (y):
                         x+y

    curried_add(10)(20)  // prints 30
  )

Naturally, keyword and optional arguments (as described in the
@seclink("keyword-arg"){next section}) work with @rhombus(fun)
expressions, too.

@section{Rest parameters with @rhombus(&)}

@doc(
  defn.specsubform 'fun $id_path($param_binding, ..., $maybe_rest) $maybe_result_annotation:
                      $body
                      ...',

  grammar maybe_rest:
    & $list_binding
    $$("ϵ"),

  grammar maybe_result_annotation:
    :: $annotation
    -: $annotation
    $$("ϵ"),
){
Defines @rhombus(id_path) as a function that may take
arbitrarily many arguments if @rhombus(&) rest is there.

@examples(
  import: math,
  fun avg(a, & bs):
    (a + math.sum(bs)) / (1 + List.length(bs)),
  avg(1, 5),
  avg(7, 9),
  avg(-2, 4),
  avg(1, 2, 5),
  avg(-8, 5, 6, 7),
)
}

@section(~tag: "keyword-arg"){Keyword and Optional Arguments}

A function argument can be made optional by using @rhombus(=) after the
argument’s pattern and providing a default-value expression after
@rhombus(=):

@(rhombusblock:
    fun scale(Posn(x, y), factor = 1):
      Posn(factor * x, factor * y)

    scale(Posn(1, 2))     // prints Posn(1, 2)
    scale(Posn(1, 2), 3)  // prints Posn(3, 6)
  )

By-keyword arguments are often useful for functions that have multiple
optional arguments. A keyword argument is indicated by prefixing a
formal or actual argument with a shrubbery keyword, which is written
with a leading @litchar{~}, and then starting a block with @rhombus(:).

@(rhombusblock:
    fun transform(Posn(x, y),
                  ~scale: factor = 1,
                  ~dx: dx = 0,
                  ~dy: dy = 0):
      Posn(factor*x + dx, factor*y + dy)

    transform(Posn(1, 2))          // prints Posn(1, 2)
    transform(Posn(1, 2), ~dx: 7)  // prints Posn(8, 2)
    transform(Posn(1, 2), ~dx: 7, ~scale: 2)  // prints Posn(9, 4)
  )

Since a keyword by itself is not allowed as an expression or pattern,
there is no possibility that a keyword will be inadvertently treated as
an actual argument or binding pattern by itself. The @rhombus(keyword)
form turns a keyword into an expression that produces the keyword, as in
@rhombus(keyword'~scale'). The @rhombus(symbol) form similarly turns an
identifier into a symbol, as in @rhombus(symbol'x').

@aside{The keyword prefix and @rhombus(=) for default values are not
 binding operators. They are specific to the syntax of @rhombus(fun).}

If an argument name is the same as its keyword (just without the
@litchar{~}), then the @rhombus(:) argument name can be omitted. That
only works for an argument that would otherwise be just an identifier
and maybe a default value, because keywords don’t work as variable names
in binding patterns.

@(rhombusblock:
    fun transform(Posn(x, y),
                  ~scale: factor = 1,
                  ~dx = 0,
                  ~dy = 0):
      Posn(factor*x + dx, factor*y + dy)
  )

