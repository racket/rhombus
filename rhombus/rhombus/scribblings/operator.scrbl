#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def op_eval = make_rhombus_eval())
@examples(
  ~eval: op_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "operator"){Operators}

The @rhombus(operator) form defines a prefix, infix, or postfix operator for
expressions, similar to a function definition:

@examples(
  ~eval: op_eval
  ~defn:
    operator (x <> y):
      Posn(x, y)
  ~repl:
    1 <> 2
  ~defn:
    operator (<<>> x):
      Posn(x, x)
  ~repl:
    <<>> 3
  ~defn:
    operator (x <<<>>>):
      Posn(x, x)
  ~repl:
    4 <<<>>>
)

An ``operator'' name does not have to be a shrubbery operator. It can be
an identifier:

@examples(
  ~defn:
    operator (x mod y):
      x - math.floor(x / y) * y
  ~repl:
    10 mod 3
)

The @rhombus(operator) form is followed by parentheses and then a
block. Inside the parentheses, there must be exactly two or three terms,
and the first, middle, or last term must be an operator or identifier to define.
The arguments can be described by binding patterns, but in that case,
they may need parentheses around the pattern to ensure that they form a
single term in next to the operator being defined:

@examples(
  ~eval: op_eval
  ~defn:
    operator ((x :: Int) <> (y :: Int)):
      Posn(x, y)
  ~repl:
    ~error:
      1 <> "apple"
)

An operator can be defined for both infix and prefix behavior in much
the same way that functions can be defined to accept one or two
arguments, and there can be multiple cases with different binding
patterns for infix, prefix, or both:

@examples(
  ~eval: op_eval
  ~defn:
    operator
    | ((x :: Int) <> (y :: Int)):
        Posn(x, y)
    | (<> (x ::Int)):
        Posn(x, x)
    | (<> "origin"):
        Posn(0, 0)
  ~repl:
    1 <> 2
    <> 3
    <> "origin"
)

A combination of prefix and postfix is also allowed, but infix and
postfix cannot be mixed. For the unusual case of infix plus postfix, you
would have to resort to more general parsing tools, as we'll see in
@secref("expr-macro").

Operator precedence is declared in relationship to other operators when
the operator is defined. With no precedence defined, @rhombus(<>) cannot
appear near an arithmetic operator like @rhombus(*):

@examples(
  ~eval: op_eval
  ~error:
    1 <> 2 * 3
  1 <> (2 * 3)
)

The initially defined operators mostly have the usual precedence:
@rhombus(*) and @rhombus(/) are stronger than @rhombus(+) and
@rhombus(-), while @rhombus(+) and @rhombus(-) have the same predence
and are left-associative. The @rhombus(*) and @rhombus(/) operators have
the same precedence as long as @rhombus(*) appears only to the left of
@rhombus(/),

A precedence declaration in @rhombus(operator) takes the form of keyword
blocks at the start of the operator's body. The possible keyword options
for prefix operators are @rhombus(~weaker_than),
@rhombus(~stronger_than), @rhombus(~same_as), or
@rhombus(~same_as_on_left). For infix operators, those options apply, as
well as @rhombus(~same_as_on_right) and @rhombus(~associativity).
Operators listed with keywords like @rhombus(~weaker_than) can be
grouped on lines however is convenient.

@examples(
  ~eval: op_eval
  ~defn:
    operator (x <> y):
      ~weaker_than: * / + -
      ~associativity: ~right
      Posn(x, y)
  ~repl:
    1 <> 2 * 3
    1 <> 2 <> 3
)

Use the keyword @rhombus(~other) in @rhombus(~weaker_than),
@rhombus(~stronger_than), or @rhombus(~same_as) to declare a precedence
relationship for operators not otherwise mentioned.

When multiple cases for an operator are provided using @vbar, then only
the first case for prefix, infix, or postfix can have options.
Precedence for prefix and infix/postfix can be independent in that form.
Alternatively, put the operator name directly after @rhombus(operator),
then start a block that contains precedence and
associativity that applies to all cases. Similar to this notation for
@rhombus(fun), a result annotation can be written before the block.

@examples(
  ~eval: op_eval
  ~defn:
    operator <> :: Posn:
      ~weaker_than: * / + -
      ~associativity: ~right
    | (x <> y):
        Posn(x, y)
    | (<> n):
        Posn(n, n)
  ~repl:
    1 <> 2 * 3
    1 <> 2 <> 3
    <> 4 * 5
)

An operator can be exported the same as an identifier:

@rhombusblock(
  export:
    <>
)

On the import side, to refer to an operator that has a prefix, put the
operator after @rhombus(.) in parentheses:

@rhombusblock(
  import:
    "posn.rhm"

  1 posn.(<>) 2
)

If the point of an operator is terseness, however, an import prefix may
defeat the point. Using a library that supplies operators may be one
reason to expose an imported name. To
selectively make an operator accessible without it import's prefix, use
the @rhombus(expose, ~impo) import modifier or a dotted import:

@rhombusblock(
  import:
    "posn.rhm".(<>)

  1 <> 2
)


@(close_eval(op_eval))
