#lang scribble/rhombus/manual

@title{Rhombus Prototype}

@section{Notation}

Numbers are decimal, either integer or floating-point, or they’re
hexadecimal integers written with @litchar{0x}:

@(rhombusblock:
    0
    42
    -42
    1_048_576
    3.14157
    .5
    6.022e23
    0xf00ba7ba2)

Identifiers are Unicode alphanumeric and @litchar{_} with an initial character
that is not numeric.

@(rhombusblock:
    pi
    scissor7
    π
    underscore_case
    camelCase)

These characters are used for shrubbery structure and are mostly not
available for use in operators:

@verbatim[~indent: 2]|{
( ) [ ] { }   ; ,   : |   « »  \   " ~  # @
}|

The @litchar{:} and @litchar{|} characters can be used as part of an operator, and any
other Unicode punctuation or symbol character is fair game for an
operator:

@(rhombusblock:
    +
    .
    ->
    >=
    !^$&%$)

To avoid potential confusion with operators alongside numbers,
however, an operator that ends in @litchar{+}, @litchar{-}, or
@litchar{.} must consist only
of that character. So, @litchar{++} and @litchar{...} are operators,
but @litchar{!+} is not.
Similar problems happen with comments, so an operator cannot contain
@litchar{//} or @litchar{/*} or have multiple characters and end in
@litchar{/}.

Keywords are like identifiers, but prefixed with @litchar{~} and no space:

@(rhombusblock:
   ~base
   ~stronger_than)

Booleans:

@(rhombusblock:
    #true
    #false)


Strings and byte strings:

@(rhombusblock:
    "This is a string, just like you’d expect"
    #"a byte string")

Comments are C-style, but block comments are nestable:

@(rhombusblock:
    before
    // This is a line comment

    /* This is a multiline
       comment that /* continues */
       on further lines */
    after)

If you need anything more at the lexeme level (such as fancier
numbers), escape to S-expression notation with @litchar|{#{}| ... @litchar|{}}|.

Shrubbery notation is whitespace-sensitive, and it uses line breaks
and indentation for grouping. A line with more indentation starts a
@emph{block}, and it’s always after a line that ends @litchar{:}, that ends @litchar{|}
(much less commonly), or that starts @litchar{|}. You can think of a
more-indented @litchar{|} as being preceded implicitly by a @litchar{:} at the end of
the previous line. A @litchar{|} counts as being indented by half a column, so
the @litchar{|}s below are indented even when they are written right under
@rhombus[if], @rhombus[match], or @rhombus[cond]:

@(rhombusblock:
    begin:
      group within block
      another group within block

    if is_rotten(apple)
    | get_another()
    | take_bite()
      be_happy()
          
    match x
    | 0:
        def zero: x
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
        wear_hat() )

A @litchar{|} is allowed only within a block, and it also starts a subblock on
the right-hand side of the @litchar{|}. Each subsequent @litchar{|} at the same
indentation creates a new subblock. Shrubbery notation allows @litchar{|} only
within a block whose groups all contain an immediate @litchar{|} subblock. A
block of @litchar{|} subblocks is an @deftech{alts-block}. A @litchar{:} isn’t needed or
allowed before the first @litchar{|} in an alts-block, because the @litchar{|}
itself is enough of an indication that a alts-block is starting.

Each line within a block forms a @deftech{group}. Groups are important,
because parsing and macro expansion are constrained to
operate on groups (although a group can contain nested blocks, etc.).
Groups at the same level of indentation as a previous line continue
that group’s block. A @litchar{|} can have multiple groups in the subblock to
its right, but the block started by @litchar{|} turns out to be the only thing
in its own group.

A @litchar{:} doesn’t have to be followed by a new line, but it starts a new
block, anyway. Similarly, a @litchar{|} that starts a block doesn’t have to be
on a new line. These examples parse the same as the previous
examples:

@(rhombusblock:
    begin: group within block
           another group within block

    if is_rotten(apple) | get_another() | take_bite()
                                          be_happy()

    match x | 0: def zero: x
                 x + zero
            | n: n + 1

    cond | is_raining(): take_umbrella()
         | going_to_beach(): wear_sunscreen()
                             take_umbrella()
         | ~else: wear_hat() )

Within a block, a @litchar{;} can be used instead of a new line to start a new
group, so these examples also parse the same:

@(rhombusblock:
    begin: group within block; another group within block

    if is_rotten(apple) | get_another() | take_bite(); be_happy()

    match x | 0: def zero: x
                 x + zero
            | n: n + 1

    cond | is_raining(): take_umbrella()
         | going_to_beach(): wear_sunscreen(); take_umbrella()
         | ~else: wear_hat()  )

You can add extra @litchar{;}s, such as at the end of lines, since @litchar{;} will
never create an empty group.

Finally, anything that can be written with newlines and indentation
can be written on a single line, but @litchar{«} and @litchar{»} may be required to
delimit a block using @litchar{«} just after @litchar{:} or @litchar{|} and @litchar{»} at the end of
the block. Normally, parentheses work just as well, but the `match`
example above illustrates a rare case where @litchar{«} and @litchar{»} would be
needed to fit on a single line. Without @litchar{«} and @litchar{»}, the following
form would put @rhombus[x + zero] insinde the definition of @rhombus[zero]:

@(rhombusblock:
    match x | 0: def zero:« x »; x + zero | n: n + 1)

Parentheses @litchar{(} ... @litchar{)}, square brackets @litchar{[} ... @litchar{]}, and curly braces
@litchar|{{}| ... @litchar|{}}| combine a sequence of groups. A comma @litchar{,} can be used to
separate groups on one line between the opener and closer.
Furthermore, a `,` is _required_ to separate groups, even if they’re
not on the same line. You can’t have extra `,`s, except after the last
group.

@(rhombusblock:
    f(1, 2,
      3, 4)

    ["apples",
     "bananas",
     "cookies",
     "milk"]

    map(add_five, [1, 2, 3, 4,]))

Indentation still works for creating blocks within @litchar{(} ... @litchar{)},
@litchar{[} ... @litchar{]}, or @litchar|{{}| ... @litchar|{}}|:

@(rhombusblock:
    map(fun (x):
          x + 5,
        [1, 2, 3, 4]) )

There are some subtleties related to the ``precedence'' of @litchar{:}, @litchar{|},
@litchar{;}, and @litchar{,}, but they’re likely to work as you expect in a given
example.
