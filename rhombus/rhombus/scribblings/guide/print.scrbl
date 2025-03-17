#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "print"){Printing Strings and Other Values}

One way to print a value to have it be the result of an expression that
is immediately with a module body.

@rhombusblock(
  #,@(hash_lang()) #,(@rhombuslangname(rhombus))
  "Hello, World!" // prints "Hello, World!" (with quotes) when run
)

This implicit printing uses @rhombus(#'expr) mode, which prints a value
as an expression that would produce that value. The @rhombus(print)
function, in contrast, uses @rhombus(#'text) mode by default, which
prints a string by writing the individual characters that make up the
string.

@margin_note_block{In documentation examples, printed results are shown in
 blue, and explicitly printed text is shown in purple. There is no
 visible distinction when running Rhombus programs.}

@examples(
  "Hello, World!"
  print("Hello, World!")
  print("Hello, World!", ~mode: #'expr)
  show("Hello, World!")
)

As the last example illustrates, @rhombus(show) is the same as
@rhombus(print), but using @rhombus(#'expr) mode.

Among predefined datatypes, only strings, byte strings, symbols, and
@tech{syntax objects} print differently in @rhombus(#'text) mode versus
@rhombus(#'expr) mode. A predefined compound datatype, such as a list or
map, prints the same in both modes, always printing elements of the
compound datatype in @rhombus(#'expr) mode. A classes that implement the
@rhombus(Printable, ~class) interface can make different choices based
on the @rhombus(mode, ~var) argument given to its
@rhombus(describe, ~datum) method.

@examples(
  print("apple")
  print(["apple", "banana", "cherry"])
)

The @rhombus(print) and @rhombus(show) functions accept any number of
arguments and print each of them with a space in between. The
@rhombus(println) and @rhombus(showln) functions are also the same, but
print a newline after all arguments. Interactive evaluation adds a
newline if needed before a @litchar{>} prompt, so we need to use
a @rhombus(block) with multiple @rhombus(print) calls to demonstrate.

@examples(
  block:
    print("hello", "there", "world")
    print("next")
  block:
    println("hello", "there", "world")
    print("next")
)

To combine multiple strings and other values into a single string for
printing, one strategy is to use the @rhombus(+&) operator, which
coerces its arguments to strings and then appends the strings.

@examples(
  println("Hello" +& ", World!")
  println("I have " +& (2 + 3) +& " fingers")
)

For building any significant amount of text, a @tech{string
 interpolation} form (see @secref("string-interpolation")) is usually a
better choice than a complex @rhombus(+&) combination.

The @rhombus(+&) operator implicity uses the @rhombus(to_string)
function, which is equivalent to printing to a string buffer and then
returning the content of that buffer as a new string. Like
@rhombus(print), @rhombus(to_string) accepts a @rhombus(~mode) argument
that defaults to @rhombus(#'text). The @rhombus(repr) function is
analogous to @rhombus(show), but it produces a string like
@rhombus(to_string).

@examples(
  to_string(42)
  to_string("apple")
  to_string("apple", ~mode: #'expr)
  repr("apple")
  "my favorite string is " +& repr("apple")
)
