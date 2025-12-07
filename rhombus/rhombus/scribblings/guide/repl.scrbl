#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def shrubbery_notation = @shrubref("top"))

@title(~tag: "repl"){Interactive Rhombus REPL}

As noted in @secref("running-cmdline"), a Rhombus
@deftech{read-eval-print loop} (@deftech{REPL}) for evaluating
individual Rhombus expressions and definitions can be started with

@nested(~style: #'inset){@exec{rhombus}}

or

@nested(~style: #'inset){@exec{racket -I rhombus}}

DrRacket also provides a REPL in its bottom pane after running a Rhombus
program, including the empty program
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus))).

One catch with Rhombus's @(shrubbery_notation) is that interactive
evaluation does not always know where to stop, because it reads
definitions and expression line-by-line. For example, with the
single-line input @rhombus(1 + 2), then a programmer almost certainly
means to add two numbers.

@examples(
  1 + 2
)

But it's also possible that a programmer means to add more numbers by
continuing with an indented @rhombus(+) on the next line.

@examples(
  1 + 2
    + 3
)

The Rhombus REPL supports only the former interaction. Naturally, the
REPL waits for multi-line input with an opening parenthesis, bracket,
bracket or quote is not yet closed.

@examples(
  (1 + 2
     + 3)
)

The REPL also looks for multiple lines when the first line contains a
@litchar{:} (not within parentheses, brackets, braces, or quotes). When
a @litchar{:} is found, the REPL continues reading until it sees a line
containing only whitespace. The @litchar{:} rule makes the following
function definition work interactively, where an extra blank line must
be entered to terminate the definition:

@examples(
  ~fake:
    fun f(x):
      x + 2
      #,("")
    fun f(x):
      x + 2
  f(1)
)

To support multi-line input where the first line does not contain
@litchar{:}, the Rhombus REPL recognizes an input that is an immediate
block, and it evaluates the sequence inside the block instead of the
block as a whole. So, enter multiline mode in the Rhombus REPL by
starting with @litchar{:}, potentially on its own line.

@examples(
  ~fake:
    :
      : 1 + 2
          + 3
          #,("")
    1 + 2 + 3
  ~error:
    match [1, 2, 3]
  ~fake:
    :
      :
        match [1, 2, 3]
        | [a, b, c]: a + b + c
          #,("")
    match [1, 2, 3]
    | [a, b, c]: a + b + c
  ~error:
    if 1 > 2
  ~fake:
    :
      :
        if 1 > 2
        | "oops"
        | "right"
      #,("")
    if 1 > 2
    | "oops"
    | "right"
  if 1 > 2 | "oops" | "right"
)
