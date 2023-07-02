#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def shrubbery_notation:
    @secref(~doc: [#'lib, "shrubbery/scribblings/shrubbery.scrbl"], "top"))

@title{Notation}

Like most languages, Rhombus syntax builds on a set of rules for parsing
characters into @deftech{tokens}. Unlike most languages–--but like Lisp,
Scheme, and Racket---Rhombus syntax uses an additional layer of rules
for grouping and nesting tokens. For languages in the Lisp family, the
intermediate structure is @deftech{S-expression notation}, which gives
Lisp its parenthesized, prefix notation. For Rhombus, the intermediate
structure is @deftech{shrubbery notation}, which is designed to
support traditional infix operators and rely on line breaks
and indentation for grouping and nesting. This section offers a brief
summary of shrubbery notation, but see @(shrubbery_notation) for
complete details.

@margin_note{To explore shrubbery notation independent of Rhombus, try
 @rhombus(#,(@hash_lang()) #,(@rhombusmodname(shrubbery))). The parsed
 form is represented as an S-expression, so the output is only useful if
 you're familiar with S-expression notation.}

Numbers are decimal, either integer or floating-point, or they’re
hexadecimal, octal, or binary integers written with a @litchar{0x},
@litchar{0o}, or @litchar{0b} prefix, respectively. An underscore can be
used to separate digits in a number.

@rhombusblock(
  0
  42
  -42
  1_048_576
  3.14157
  .5
  6.022e23
  0xf00ba7ba2
  0o377
  0b1001
)

Identifiers use Unicode alphanumeric characters, @litchar{_}, and
emoji sequences, with an initial character that is not numeric.

@rhombusblock(
  pi
  scissor7
  π
  underscore_case
  camelCase
)

Keywords are like identifiers, but prefixed with @litchar{~} and no
space. As a datatype distinct from identifiers, they are useful as names
that cannot be misconstrued as bound variables or as any other kind of
expression form.

@rhombusblock(
   ~base
   ~stronger_than
)

The following characters are used for shrubbery structure and are
mostly not available for use in operators:

@verbatim(~indent: 2)|{
( ) [ ] { } '   ; ,   : |   « »  \   "  # @
}|

Any other Unicode punctuation or symbol character (but not an emoji) is
fair game for an operator:

@rhombusblock(
  +
  .
  ->
  >=
  !^$&%$
)

The @litchar{:} and @litchar{|} characters can be used as part of an
operator, even though the characters have a special meaning when used alone. To avoid
confusion with @tech{blocks}, an operator cannot end with @litchar{:}
unless it contains only @litchar{:} characters. Similarly, to avoid
potential confusion with operators alongside numbers, an operator that
ends in @litchar{+}, @litchar{-}, or @litchar{.} must consist only of
that character. So, @litchar{++} and @litchar{...} are operators, but
@litchar{!+} is not. Similar problems happen with comments, so an
operator cannot contain @litchar{//} or @litchar{/*} or have multiple
characters and end in @litchar{/}. A @litchar{~} cannot be used by
itself as an operator to avoid confusion with @litchar{~} to form a
keyword.

Shrubbery notation does not include a notion of operator precedence.
Instead, Rhombus builds a precedence-parsing layer on top of shrubbery
notation (which is why shrubberies are not full-grown trees). Precedence
in Rhombus is macro-defined in the same way that syntactic forms are
macro-defined in Rhombus.

Booleans are written with a leading @litchar{#} followed immediately by
@litchar{true} or @litchar{false}.

@rhombusblock(
  #true
  #false
)


Strings of Unicode characters use single quotes, and byte strings are
similar, but with a @litchar{#} prefix. Strings and byte string support
the usual escapes, such as @litchar{\n} for a newline character or byte.

@rhombusblock(
  "This is a string,\n just like you’d expect"
  #"a byte string"
)

Comments are C-style, but block comments are nestable.

@(
  rhombusblock_etc:«
  // This is a line comment

  /* This is a multiline
     comment that /* continues */
     on further lines */
  »
)

To aid interoperability with Racket and to support some rarely useful
datatypes, such as characters, shrubbery notation includes an escape to
S-expression notation through @(s_exp_braces). For example,
@litchar{#{list-first}} is a single identifier that includes @litchar{-}
as one of its characters. A @(s_exp_braces) cannot wrap a
list-structured S-expression that uses immediate parentheses, however.

Shrubbery notation is whitespace-sensitive, and it uses line breaks and
indentation for grouping. A line with more indentation starts a
@deftech{block}, and it’s always after a line that ends @litchar{:}. A
@litchar{|} alternative also starts a block, and the @litchar{|} itself
can start a new line, in which case it must line up with the start
of its enclosing form. So, the @litchar{|}s below are written with the
same indentation as @rhombus(if), @rhombus(match), or @rhombus(cond) to
create the alternative cases within those forms:

@margin_note{In DrRacket, hit Tab to cycle through the possible
 indentations for a line. See also @shrubref("drracket-shrubbery").}

@rhombusblock(
  block:
    println("group within block")
    println("another group within block")

  if is_rotten(apple)
  | get_another()
  | take_bite()
    be_happy()
        
  match x
  | 0:
      def zero = x
      x + zero
  | n:
      n + 1

  cond
  | // check the weather
    is_raining():
      take_umbrella()
  | // check the destination
    going_to_beach():
      wear_sunscreen()
      take_umbrella()
  | // assume a hat is enough
    ~else:
      wear_hat()
)

A @litchar{:} isn’t needed before the first @litchar{|} in an
alts-block, because the @litchar{|} itself is enough of an indication
that a sequence of alternatives is starting, but a @litchar{:} is
allowed. Some forms support the combination of a @litchar{:} followed by
a sequence of @litchar{|} alternatives, but most forms have either a
@litchar{:} block or a sequence of @litchar{|} alternatives.

Each line within a block forms a @deftech{group}. Groups are important,
because parsing and macro expansion are constrained to operate on groups
(although a group can contain nested blocks, etc.). Groups at the same
level of indentation as a previous line continue that group’s block. A
@litchar{|} can have multiple groups in the subblock to its right, but
the block started by @litchar{|} turns out to be the only thing in its
own group. A @litchar{:} block or sequence of @litchar{|} alternatives
can only be at the end of an enclosing group.

A @litchar{:} doesn’t have to be followed by a new line, but it starts a
new block, anyway. Similarly, a @litchar{|} that starts an alternative
doesn’t have to be on a new line. These examples parse the same as the
previous examples:

@rhombusblock(
  block: group within block
         another group within block

  if is_rotten(apple) | get_another() | take_bite()
                                        be_happy()

  match x | 0: def zero: x
               x + zero
          | n: n + 1

  cond | is_raining(): take_umbrella()
       | going_to_beach(): wear_sunscreen()
                           take_umbrella()
       | ~else: wear_hat()
)

Within a block, a @litchar{;} can be used instead of a new line to start
a new group, so these examples also parse the same:

@rhombusblock(
  block: group within block; another group within block

  if is_rotten(apple) | get_another() | take_bite(); be_happy()

  match x | 0: def zero: x
               x + zero
          | n: n + 1

  cond | is_raining(): take_umbrella()
       | going_to_beach(): wear_sunscreen(); take_umbrella()
       | ~else: wear_hat()
)

You can add extra @litchar{;}s, such as at the end of lines, since
@litchar{;} will never create an empty group.

Finally, anything that can be written with newlines and indentation can
be written on a single line, but @litchar{«} and @litchar{»} may be
required to delimit a block using @litchar{«} just after @litchar{:} or
@litchar{|} and @litchar{»} at the end of the block. Normally,
parentheses work just as well, but the @rhombus(match) example above illustrates
a rare case where @litchar{«} and @litchar{»} would be needed to fit on
a single line. Without @litchar{«} and @litchar{»}, the following form
would put @rhombus(x + zero) inside the definition of @rhombus(zero):

@rhombusblock(
  match x | 0: def zero:« x »; x + zero | n: n + 1
)

Parentheses @parens, square brackets @brackets, and curly braces @braces combine a
sequence of groups. A comma @litchar{,} can be used to separate groups
on one line between the opener and closer. Furthermore, a @litchar{,} is
@emph{required} to separate groups, even if they’re not on the same line. You
can’t have extra @litchar{,}s, except after the last group.

@rhombusblock(
  f(1, 2,
    3, 4)

  ["apples",
   "bananas",
   "cookies",
   "milk"]

  map(add_five, [1, 2, 3, 4,])
)

Indentation still works for creating blocks within @parens,
@brackets, or @braces:

@rhombusblock(
  map(fun (x):
        x + 5,
      [1, 2, 3, 4])
)

There are some subtleties related to the ``precedence'' of @litchar{:},
@litchar{|}, @litchar{;}, and @litchar{,}, but they’re likely to work as
you expect in a given example.

Single-quote marks @quotes are used for quoting
code (not strings), as in macros. Quotes work like @parens, except
that the content is more like a top-level or block sequence, and
@litchar{;} is used as a group separator (optional when groups are on
separate lines).

@rhombusblock(
  rule 'thunk: $body':
    'fun (): $body'
)

Nested quoting sometimes requires the use of @litchar{'} @litchar{«} ...
@litchar{»} @litchar{'} so that the nested opening quote is not parsed
as a close quote. This counts as a different use of @litchar{«} and
@litchar{»} than with @litchar{:} or @litchar{|}, and it doesn't
disable indentation for the quoted code.
