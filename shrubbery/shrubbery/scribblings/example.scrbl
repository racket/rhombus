#lang scribble/rhombus/manual
@(import:
    "rkt-id.rkt" as rkt
    meta_label:
      rhombus open)

@title(~tag: "example"){Examples}

Here are some example shrubberies. Each line either uses old indentation
to continue a nesting level that was started on a previous line, starts
with new indentation and follows a line that ends with @litchar{:}, or
starts with new indentation and a @litchar{|} on the same line. A
@litchar{:} or @litchar{|} can also appear in the middle of a line, but
that's roughly a shorthand for starting a new indented line after the
@litchar{:} or before the @litchar{|}. The complete rules involve more
terminology, but that's enough to get a sense of the examples.

@rhombusblock(
  fun identity(x): x

  fun fib(n):
    match n
    | 0: 0
    | 1: 1
    | n: fib(n-1) + fib(n-2)

  fun print_sexp(v):
    match v
    | []: print("()")
    | [fst, & rst]:
        print("(")
        print_sexp(fst)
        for (v: rst):
          print(" ")
          print_sexp(v)
        print(")")
    | Pair(fst, snd):
        print("(")
        print_sexp(fst)
        print(" . ")
        print_sexp(snd)
        print(")")
    | v: print_atom(v)
)

Forms like @rhombus(fun), @rhombus(match), and @rhombus(for) are not
specified by shrubbery notation, since specifying those forms is up to a
language that is built on top of shrubbery notation. Still, shrubbery
notation is meant to accommodate a particular kind of syntax for nested
blocks (via @litchar{:} and indentation) and conditional blocks (via
@litchar{|}).

Identifiers are C-style with alphanumerics and underscores. Operators
are sequences of symbolic characters in the sense of
@(rkt.char_is_symbolic), roughly. No spaces are needed between
operators and non-operators, so @litchar{1+2} and @litchar{1 + 2} mean
the same thing. Comments are C-style. See @secref("token-parsing")
for more information.

The following tokens are used for grouping, in addition to line breaks
and indentation:

@verbatim(~indent: 2){
( ) [ ] { } '  ; ,   : |   « »  \
}

Parentheses, square brackets, and curly braces are used to delimit
groups in the obvious way. A @litchar{'} is used in a parenthesis-like
way, too, and it's intended for quoting shrubbery terms.
A @litchar{;} or @litchar{,} acts as a group
separator, even within a single line. A @litchar{:} or @litchar{|}
treats remaining item on the same line like a new indented line, which
forms a block of nested groups. A guillemet pair @litchar{«} and @litchar{»} can be
used in rare cases to explicitly bracket subgroups formed by
@litchar{:} and @litchar{|} without line breaks. A @litchar{\} continues
a line, effectively shifting all columns on the next line as if they
appeared immediately after the @litchar{\}.
