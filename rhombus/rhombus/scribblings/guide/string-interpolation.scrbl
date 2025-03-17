#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def self_source:
    let url = "https://github.com/racket/rhombus/blob/master/"
    let path = "rhombus/rhombus/scribblings/guide/string-interpolation.scrbl"
    @hyperlink(url ++ path){source to this document})

@title(~tag: "string-interpolation"){String Interpolation}

@deftech{String interpolation} starts with literal text, but it adds an
escape syntax to allow an arbitrary expression that produces text to
substitute in place of the escape. In Rhombus, string interpolation is
implemented by a combination of shrubbery notation and library
functions:

@itemlist(

 @item{Shrubbery @litchar("@") notation provides a general way to write
 text within @braces with minimal escaping of special characters. It also
 provides a general escape from (literal) text mode back to shrubbery
 (expression) mode; the escape form uses @litchar("@").

 A @litchar("@") form is desugared at the shrubbery layer, not the
 langauge layer; it is always equivalent to a function-call form. Text
 plus escapes turn into a list argument in the function call.}

 @item{@rhombus(str) and related functions are designed to be called
 using @litchar("@") notation. They expect a list argument containing
 text produced by literal strings and other values produced by arbitrary
 expressions.}

)

For example, the shrubbery form

@rhombusblock(@str{Hello})

is equivalent to the shrubbery form

@rhombusblock(str(["Hello"]))

Within a @rhombus(#,(hash_lang()) #,(@rhombuslangname(rhombus))) module,
both of those are a call to the @rhombus(str) function, which simplify
converts every element of the list to a string and then appends the
strings.

The shrubbery form

@rhombusblock(@str{1 + 1 is @(1 + 1).})

is equivalent to

@rhombusblock(str(["1 + 1 is ", (1 + 1), "."]))

which illustrates how @litchar("@") acts as an escape within the
literal-text @braces of an enclosing @litchar("@") form.

The use of @litchar("@") to start a literal-text form an to escape
within literal text are actually the same @litchar("@"), just in (1)
function-call form with a function @rhombus(str) to call, and (2)
parenthesized-expression form, with no function to call immediately
after @litchar("@"). Another option is (3) a function to call
immediately after @litchar("@") and the regular arguments in parentheses
after that function name. Any of those @litchar("@") can be used in a
consistent way both inside and outside of literal-text @braces.

@examples(
  @str{1 + 1 is @(1 + 1).}
  @(1 + 1)
  @List{apple}
  @List(1, 2, 3)
  @str{1 + 1 is @str{2}.}
  @str{1 + 1 is @math.sum(1, 1).}
  @str{Text within {} is
       not constrained to
       a single line.}
)

Within @braces after @litchar("@"), any character counts as literal text
except @litchar("@"), @litchar("{"), and @litchar("}"). Even those
characters can be treated as literal by using a locally unique
opener-closer pair, such as @litchar("|-{") and @litchar("}-|").

@examples(
  @str|-{The example above: @str{1 + 1 is @(1 + 1).}}-|
)

In general, an opener is either the usual @litchar("{") or it starts
with @litchar("|") and ends with @litchar("{") with other characters in
between, and the closer is formed by reversing that sequence and
flipping parenthesis-like shapes. See
@secref("at-notation", ~doc: shrub_doc) and
@secref("at-parsing", ~doc: shrub_doc) for more information on those
pairs and about @litchar("@") notation generally.

Functions like @rhombus(str.d) and @rhombus(str.f) are meant to be used
with escapes inside the argument of @rhombus(str) to format numbers.

@examples(
  ~repl:
    @str{The key @repr(#'pi) is mapped to @str.f(math.pi, ~precision: 2).}
  ~defn:
    fun pct(n): str.f(n * 100, ~precision: 1) ++ "%"
  ~repl:
    print(@str{Contents: @pct(0.251) apples
                         @pct(1/6) bananas
                         @pct(0.5) cherries})
)

See @rhombus(str.d) and @rhombus(str.f) for additional examples.

String interpolation is just one use of shrubbery @litchar("@")
notation. The same notation is used for writing documentation,
@margin_note{See the @self_source, for example.} creating slide
presentations, and other tasks that involve lots of prose. Those many
uses make shrubbery's @litchar("@") notation worthwhile.
