#lang rhombus/scribble/manual
@(import:
    "rkt-id.rkt" as rkt
    meta_label:
      rhombus open)

@(fun open_close(o, c):
    @elem{@litchar(o)…@litchar(c)})

@(def parens = @open_close("(", ")"))
@(def brackets = @open_close("[", "]"))
@(def braces = @open_close("{", "}"))
@(def s_exp_braces = @open_close("#{", "}"))
@(def quotes = @open_close("'", "'"))
@(def explicit_quotes = @open_close("'«", "»'"))
@(def comma = @litchar(","))
@(def colon = @litchar(":"))
@(def vbar = @litchar{|})


@title(~tag: "example"){Quick Overview}

Shrubbery notation consists of

@itemlist(

 @item{simple @tech{terms} like numbers, identifiers and strings;}

 @item{@tech{groups}, which roughly correspond to lines or to
 @(comma)-separated elements within @parens, @brackets, or @braces;}

 @item{@tech{blocks}, which are written after @colon or @vbar and are
 more indented than surrounding text; and}

 @item{@tech{alternatives}, which combine a sequence of @(vbar)s (that contain
 blocks).}

)

@margin_note_block{To explore shrubbery notation independent of a
 language like Rhombus that uses the notation, try
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(shrubbery))). The parsed
 form is represented as an S-expression, so the output is only useful if
 you're familiar with S-expression notation.}

@section{Simple Terms}

Shrubbery numbers are decimal, either integer or floating-point, or they're
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
unless it contains only @litchar{:} characters. Similar problems happen with comments, so an
operator cannot contain @litchar{//} or @litchar{/*}. A @litchar{~} cannot be used by
itself as an operator, which avoids confusion with @litchar{~} to start a
keyword.

Shrubbery notation does not include a notion of operator precedence.
Instead, a language like Rhombus builds a precedence-parsing layer on top of shrubbery
notation (which is why shrubberies are not full-grown trees).

Booleans are written with a leading @litchar{#} followed immediately by
@litchar{true} or @litchar{false}.

@rhombusblock(
  #true
  #false
)

Strings of Unicode characters use double quotes, and byte strings are
similar, but with a @litchar{#} prefix. Strings and byte strings support
the usual escapes, such as @litchar{\n} for a newline character or byte.

@rhombusblock(
  "This is a string,\n just like you'd expect"
  #"a byte string"
)

Multi-line strings with escapes are supported through
@litchar("@")…@braces notation, where literal text is written within
@braces, and @litchar("@") also serves as an escape back to shrubbery
notation within @braces. This form is typically used for string
interpolation and documentation prose. See @secref("at-parsing") for
more information.

@section{Comments}

Comments are C-style, except that block comments are nestable.

@(
  rhombusblock_etc:«
  // This is a line comment

  /* This is a multiline
     comment that /* continues */
     on further lines */
  »
)

The @litchar("#//") form comments out a subsequent @tech{group}. See
@secref("group-comment") for more information.

@section{Lines and Indentation}

Shrubbery notation is whitespace-sensitive, and it uses line breaks and
indentation for grouping. An indented line that starts with an operator
continues the previous line:

@rhombusblock(
  very_long_variable_name
    + also_very_long_variable_name
)

Otherwise, a line with more indentation starts a
@tech{block}, and it's normally after a line that ends with a @litchar{:}.
A @litchar{|} alternative also starts a block, and the @litchar{|} itself
can start a new line, in which case it must line up with the start
of its enclosing form. So, the @litchar{|}s below are written with the
same indentation as @rhombus(if), @rhombus(match), or @rhombus(cond) to
create the alternative cases within those forms:

@margin_note_block{In DrRacket, hit Tab to cycle through the possible
 indentations for a line, or use Shift-Tab to cycle in reverse order.
 See also @secref("drracket-shrubbery").}

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
      let zero = x
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

A @litchar{:} isn't needed before the first @litchar{|} in a sequence of
alternatives, because the @litchar{|} itself is enough of an indication
that a sequence of alternatives is starting, but a @litchar{:} is
allowed if it ends a line before @litchar{|}. The combination of a @litchar{:} block followed by
lines and then a sequence of @litchar{|} alternatives is allowed, but more typically, one of the other of a
@litchar{:} block or a sequence of @litchar{|} alternatives is used.

Each line within a block forms a @tech{group}. Groups are important,
because parsing and macro expansion for a shrubbery-based language are
normally constrained to operate on groups
(although a group can contain nested blocks, etc.). Groups at the same
level of indentation as a previous line continue that group's block. A
@litchar{|} can have multiple groups in the nested block to its right.
A @litchar{:} block or sequence of @litchar{|} alternatives
can only be at the end of an enclosing group.

A @litchar{:} doesn't have to be followed by a new line, but it starts a
new block, anyway. Similarly, a @litchar{|} that starts an alternative
doesn't have to be on a new line. These examples parse the same as the
previous examples:

@rhombusblock(
  block: group within block
         another group within block

  if is_rotten(apple) | get_another() | take_bite()
                                        be_happy()

  match x | 0: let zero = x
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

  match x | 0: let zero = x; x + zero
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
parentheses work just as well, since they can be wrapped around any
expression---but definitions, for example, can create a situation
where @litchar{«} and @litchar{»} are needed to fit on
a single line. Without @litchar{«} and @litchar{»}, the following form
would put @rhombus(x + zero()) inside the definition of @rhombus(zero):

@rhombusblock(
  match x | 0: fun zero():« x »; x + zero() | n: n + 1
)


@section{Parentheses, Brackets, Braces, and Quotes}

Parentheses @parens, square brackets @brackets, and curly braces @braces combine a
sequence of groups. A comma @litchar{,} can be used to separate groups
on one line between the opener and closer. Furthermore, a @litchar{,} is
@emph{required} to separate groups, even if they're not on the same line. You
can't have extra @litchar{,}s, except after the last group.

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

Single-quote marks @quotes are used for quoting
code (not strings), as in macros. Quotes work like @parens, except
that the content is more like a top-level or block sequence, and
@litchar{;} is used as a group separator (optional when groups are on
separate lines).

@rhombusblock(
  macro 'thunk: $(body :: Block)':
    'fun () $body'
)

Nested quoting sometimes requires the use of @explicit_quotes
so that the nested opening quote is not parsed
as a close quote. This counts as a different use of @litchar{«} and
@litchar{»} than with @litchar{:} or @litchar{|}, and it doesn't
disable indentation for the quoted code.

@section{S-Expression Interoperability}

To aid interoperability with Racket and to support some rarely useful
datatypes, such as characters, shrubbery notation includes an escape to
S-expression notation through @(s_exp_braces). For example,
@litchar{#{list-first}} is a single identifier that includes @litchar{-}
as one of its characters. A @(s_exp_braces) cannot wrap a
list-structured S-expression that uses immediate parentheses, however.
Keywords can also be written this way, prefixed with @litchar{~}, like
@litchar{~#{immutable?}}.
