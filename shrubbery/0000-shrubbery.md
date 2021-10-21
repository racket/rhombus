- Feature Name: Shrubbery notation
- Start Date: 2019-10-01
- RFC PR: [racket/racket2-rfcs#122](https://github.com/racket/racket2-rfcs/pull/122)

# Summary
[summary]: #summary

Shrubbery notation is similar to S-expression notation, but instead of
generating fully formed trees, it is intended to partially group input
for further enforestation by another parser (e.g., as in Honu). The
notation is line- and indentation-sensitive, and the parsed form of a
shrubbery imposes grouping to ensure that further parsing is
consistent with the shrubbery's lines and indentation.

# Motivation
[motivation]: #motivation

S-expression notation imposes a grouping at the lexeme level that is
all but guaranteed to be respected by further parsing via macro
expansion. One consequence of this lexeme-based grouping is that
programs can be pretty-printed and textually traversed in standard
ways.

A traditional use of S-expression notation, however, insists that
*all* grouping is reflected in the S-expression. Reifying all grouping
at the lexeme level is so onerous that many practical deployments of
S-expressions include deviations from the rule, such as keyword-based
arguments or implicit grouping by position (as in various Clojure
forms).

Another disadvantage of S-expressions is that many of the parentheses
are redundant after the expression is pretty-printed, because
indentation provides the same grouping information in a more
human-readable way. That observation suggests instead relying on line
breaks and indentation to impart grouping information, as in Python.

Shrubbery notation explores a point in the design space where the
notation is

 - line- and indentation-sensitive, and
 - intended to constrain grouping but not reflect every detail of grouping.

Deferring complete grouping to another parser relieves a burden on
reader-level notation. At the same time, line- and
indentation-sensitive rules constrain parsing to ensure that line
breaks and indentation in the source are not misleading.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Here are some example shrubberies. Each line either uses old
indentation to continue a nesting level that was started on a previous
line, starts with new indentation and follows a line that ends with
`:`, or starts with new indentation and a `|` on the same line. A `:`
or `|` can also appear in the middle of a line, but that's roughly a
shorthand for starting a new indented line after the `:` or before the
`|`. The complete rules involve more terminology, but that's enough to
get a sense of the examples.

```
def identity(x): x

def fib(n):
  cond
  | n == 0: 0
  | n == 1: 1
  | else: fib(n-1) + fib(n-2)

def print_sexp(v):
  match v
  | empty: display("()")
  | cons(a, d):
      if is_list(d)
      | display("(")
        print_sexp(a)
        for (v = in_list(d)):
          display(" ")
          print_sexp(v)
        display(")")
      | display("(")
        print_sexp(a)
        display(". ")
        print_sexp(d)
        display(")")
  | v: print_atom(v)
```

Forms like `def`, `cond`, and `match` are not specified by
shrubbery notation, since specifying those forms is up to a language
that is built on top of shrubbery notation. Still, shrubbery notation
is meant to accommodate a particular kind of syntax for nested blocks
(via `:` and indentation) and conditional branches (via `|`).

Identifiers are C-style with alphanumerics and underscores. Operators
are sequences of symbolic characters in the sense of `char-symbolic?`,
roughly. No spaces are needed between operators and non-operators, so
`1+2` and `1 + 2` mean the same thing. Comments are C-style, plus a
`#//` group-comment form. See [lexeme parsing](#lexeme-parsing) for
more information.

The following tokens are used for grouping, in addition to line breaks
and indentation:

```
( ) [ ] { }  ; ,   : |   « »  \
```

Parentheses, square brackets, and curly braces are used to form groups
in the obvious way. A `;` or `,` acts as a group separator, even
within a single line. A `:` or `|` treats remaining item on the same
line like a new indented line, which forms a subgroup. A guillemet
pair `«` and `»` can be used (probably very rarely) to explicitly
bracket subgroups formed by `:` and `|` without line breaks. A `\`
continues a line, effectively shifting all columns on the next line as
if they appeared immediately after the `\`.

## Grouping by lines

The main grouping rule is that sequences on different lines with the
same indentation create separate groups, one for each line.

```
this is the first group
this is the second group
```

Comments and lines with only whitespace are ignored. They don't count
when this document says “the previous line” or “the next line.”

## Grouping by _opener_-_closer_ pairs

An _opener_-_closer_ pair `(` and `)`, `[` and `]`, or `{` and `}`
forms a nested group that can span lines. Within the _opener_-_closer_
pair, `,` sparates groups. Groups can be on separate lines at the same
indentation, but groups on separate lines still must be separated by
`,`. Parsing retains whether a subgroup is formed by `()`, `[]`, or
`{}`.

```
group 1
[group 2 - subgroup I,
 group 2 - subgroup II,
 (group 2 - subgroup III - subsubgroup A,
  group 2 - subgroup III - subsubgroup B,
  {group 2 - subgroup III - subsubgroup C, subsubsubgroup α,
   group 2 - subgroup III - subsubgroup C, subsubsubgroup β})]
(group 3 - subgroup I,  group 3 - subgroup II,
 group 3 - subgroup III)
```

The following three forms are not allowed, because they are missing a
`,` between two groups:

```
// Not allowed
(1
 2)
[1
 2]
{1
 2}
```

A `,` is disallowed if it would create an empty group, except that a
trailing `,` is allowed.

```
// Not allowed
(, 1)
(1,, 2)

// Allowed, but not standard
(1, 2,)
```

A trailing `,` is only standard style when the _closer_ that follows is
on its own line.

```
list(
  red,
  green,
  blue,
  orange,
)
```

## Blocking with `:` and indentation

A sequence of groups has a particular indentation that is determined
by the first group in the sequence. Subsequent groups in a sequence
must start with the same indentation as the first group.

```
group 1
group 2
// error, because the group is indented incorrectly:
  group 3
```

When a line ends with `:` and the next line is more indented, then
it starts a new sequence of groups that form a _block_:

```
group:
  subgroup 1
  subgroup 2
```

There is no constraint on how much indentation a nested group sequence
must use, as long as the indentation is more than the enclosing group.
Also, a new line is not required after `:`, but then it's as if the
`:` is followed by a newline plus spaces that reach the same column as
the `:`. All four of the following groups are the same, each with one
block that has two nested groups:


```
hello:
 world
 universe

hello:
       world
       universe

hello: world
       universe

hello:    world
          universe
```

Within an _opener_-_closer_ pair, a nested group sequence can start at
any indentation; it doesn't have to be indented to the right of the
_opener_.

```
function(
  argument,
  more
)
```

A block that is started with `:` can be empty, and `:` can be used at
the start of a group so that the group contains only a block. For
example, the first of the following two top-level groups has `empty`
followed by a block with zero groups, and the second top-level group
has just a block that contains one group with the single element
`untagged`:

```
empty:

: untagged
```

## Continuing with indentation and an operator

When a newly indented line starts with an operator and when the
preceding line does _not_ end with `:`, then the indented line does
not form a block, and it may instead continue the previous line. The
operator-starting line continues only if the previous line was not a
continuing line; however, additional continuing lines can start with
an operator (not necessarily the same one) at the same indentation as
the original continuing line. The following two groups are the same:

```
f(1) + 2
  + 3 + 4
  - 5 - 6

f(1) + 2 + 3 + 4 - 5 - 6
```

A block is always at the end of its immediately containing group. One
consequence is that an operator-starting line cannot continue a group
that already has a block:

```
hello: world
  + 3 // bad indentation
```

Along those lines, there is no ambiguity when an indented line appears
after `:` and starts with an operator. In that case, the indented line
is part of the block, since it cannot continue the group that contains
the block. For example, the following two groups are the same, each
with a block that has a `+ 3` group:

```
hello: + 3

hello:
  + 3
```

## Blocking with `|`

A `|` is implicitly shifted half a column right (so, implicitly
nested), and it is implicitly followed by a `:` that conceptually
occupies same column as the `|`. That is, like `:`, a `|` always
creates a nested block. Furthermore, `|` starts an enclosing block
that includes the `|` block plus subsequent `|` blocks that are at the
same indentation. A `|` that starts the enclosing block can appear at
the start of a line with new indentation. The following four groups
are the same:

```
hello
| world
| universe

hello
  | world
  | universe

hello | world
      | universe

hello |
        world
      |
        universe
```

Each of the four groups has two elements: `hello` and a block. The
block has two groups, each of which is a more nested block. The first
nested block has `world` in a single group, and the second nested
block as `universe` in a single group.

A `|` cannot be a in a top-level sequence of groups or start a group
immediately within `()`, `[]`, or `{}`, and it cannot appear just
after `:`.

If a `|` appears on the same line as an earlier `|` and is not more
nested inside `()`, `[]`, or `{}`, then the `|` terminates the earlier
`|`'s block and continues its enclosing block with a new `|` group.
The intent and consequence of this rule is that multiple `|`s can be
used on a single line as an alternative to starting each `|` on its
own line, making the following groups the same as the above groups:

```
hello | world | universe

hello
| world | universe
```

The implicit shifting of `|` by half a column is consistent with its
visual representation, and it avoids the possibility of a group
sequence that contains a mixture of `|`-started groups and other kinds
of groups. Standard indentation uses no additional space of
indentation before `|` relative to its enclosing block's group.


## Separating groups with `;` and `,`

A `;` separates two groups on the same line. A `;` is allowed in any
context—except between groups immediately within, `()`, `[]`, or `{}`,
where a `,` separates groups. The following three blocks
are the same:

```
hello:
  world
  universe  

hello:
  world; universe

hello: world; universe
```

The `;` and `,` separators interact differently with blocks formed by
`:` and `|`. A `,` closes subgroups and blocks as necessary to reach
an enclosing `()`, `[]`, or `{}`, while a `;` separate groups within a
nested group sequence. If `;` would create an empty group, it is
ignored.

For example, the following two groups are the same, and they have one
parenthesized term that has a single block, and the block has two
groups:

```
(hello: world; universe)

(hello: world
        universe)
```

The following two groups are also the same, where the group has one
parenthesized term, but that term contains two groups, where the first
group is a block that contains a single group:


```
(hello: world, universe)

(hello: world,
 universe)
```

## Delimiting blocks with `«` and `»`

Many shrubbery forms can be written with `:`, `|`, and `;` on a single
line. For example, the following two groups are the same:

```
hello: if x | world; planet | universe

hello:
  if x
  | world
    planet
  | universe
```

Not all forms can be collapsed to a single line without extra
delimiters, however. For example, these six groups are all different:

```
outside:
  inside: fruit
  rind

// not the same, because `rind` is within `inside:`
outside: inside: fruit; rind

if true
| if false
  | x
  | y
| z

// not the same, because there's one block with five `|` alternatives
if | true | if false | x | y | z

hello:
  if x
  | world
  | universe
  the end

// not the same, because `the end` is in the second `|`:
hello: if x | world | universe; the end
```

A block can be delimited explicitly with `«` and `»` to indicate where
it ends, and any form can be written on a single line using `«` and
`»`. A `«` can be used immediately after `:` or immediately after `|`,
and a `»` indicates the end of the block that starts after the `:` or
`|`.

Each pair of groups here represent the same group:

```
outside:
  inside: fruit
  rind

outside: inside:« fruit »; rind

if true
| if false
  | x
  | y
| z

if | true |« if false | x | y » | z

hello:
  if x
  | world
  | universe
  the end

hello: if x | world |« universe »; the end
```

Delimiting blocks with `«` and `»` is expected to be rare in practice,
both because programmers are likely to break things across lines and
because a language that uses shrubbery notation is likely to allow
`()` in places where grouping might be needed. For example, assuming
that `if` is an expression form and `()` can wrap an expression, a
nested conditional is probably better written like this:

```
if | true | (if false | x | y) | z
```

Using `()` in this way does not produce an equivalent shrubbery to `if
| true |« if false | x | y »| z`, but it might represent an equivalent
expression in the language using shrubbery notation.

To stay consistent with blocks expressed through line breaks and
indentation, a block with `«` and `»` must still appear at the end of
its enclosing group.

```
// not allowed, because a block must end a group
inside:« fruit » more
```


## Continuing a line with `\`

As a last resort, `\` can be used at the end of a line (optionally
followed by whitespace and coments on the line) to continue the next
line as it if were one line continuing with the next line. The itself
`\` does not appear in the parsed form. A that is not at the end of a
line (followed by whitespace and coments) is treated the same as
whitespace.

Lines contianing only whitespace and (non-term) comments do not count
as “the next line” even for `\` continuations, so any number of
whitespace and comment lines can appear between `\` and the line that
it continues.


```
this is \
  the first group
this \ is \ the \ second \ group

this is a group \
 with (a,
                       nested,
                       list)

this is a group \
 with (a,
                \
       nested,
                \
       list)

this is a group \
 with (a,
                \
       /* this a comment on `nested`: */
       nested,
                \
       list)

```

## Group comments

When `#//` appears on its own line (or with only whitespace and
non-group comments), then its indentation does not matter, and it
comments out the next group—witch might be a single-line group, block,
or `|` block. A `#//` is not allowed without a group afterward to
comment out, and `#//`s do not nest (i.e., two `#//`s in a row is
always an error.

The following three groups all parse the same:

```
{
  hello:
    val x: f(1, 2 + 3)
    match x
    | 1: 'one'
    | 2: 'two'
}

{
  hello:
    val x:
      #//
      g(-1)
      f(
        #//
        0,
        1,
        2 + 3,
        #//
        4 + 5)
    #//
    not included in the code
    match x
    #//
    | 0: no
    | 1: 'one'
    #//
    | 1.5: no
    | 2: 'two'
    #//
    | 3: no,
  #//
  goodbye:
    the enclosing group of the block is commented out
}

{
  hello:
    val x:
      #//
      g(-1)
      f(
        #//
        0,
        1,
        2 + 3,
        #//
        4 + 5)
    #//
    not included in the code
    match x
    #//
    | 0: no
    | 1: 'one'
    #//
    | 1.5: no
    | 2: 'two'
    #//
    | 3: no,
  #//
  goodbye:
    the enclosing group of the block is commented out
}
```

Because it must be on it own line, the `#//` group commenting-form is
limited in its reach. Neverthess, it is expected to be useful for
commenting out definitions and conditional branches.

## More examples

Here are more example shrubberies. These shrubberies are not
necessarily consistent with each other in the sense of sketching a
single language that uses shrubbery notation; they show different
potential ways of using the notation.


```
define pi: 3.14

define
| fib(0): 0
| fib(1): 1
| fib(n): fib(n-1) + fib(n-2)

define fib(n):
  match n
  | 0: 0
  | 1: 1
  | n: fib(n-1) + fib(n-2)

define fib(n):
  match n | 0: 0
          | 1: 1
          | n: (fib(n-1)
                  + fib(n-2))

define fib(n):
  match n
  | 0:
      0
  | 1:
      1
  | n:
      fib(n-1) + fib(n-2)

define make_adder(n):
  lambda (m):
    printf("adding to ~a\n", m)

define fourth(n: integer):
  define m: n*n
  define v: m*m
  printf("~a^4 = ~a\n", n, v)
  v

struct posn(x, y):
  property prop_equal_and_hash:
    let (hc = lambda (a: posn, hc):
                hc(a.x) + hc(a.y),
         eql = lambda (a: posn, b: posn, eql):
                 eql(a.x, b.x) && eql(a.y, b.y)):
      values(eql, hc, hc)

define go():
  define helper(n):
    list(n, n)
  define more(m):
    if m == 0 | "done"
              | more(m - 1)
  helper(more(9))

define curried:
  lambda (x):
    lambda (y):
      lambda (z):
        list(x, y, z)

let (x = 1,
     y = 2):
  printf("About to add")
  x+y

define show_zip(l, l2):
  for (x = in_list(l),
       x2 = in_list(l2)):
    print(x)
    print_string(" ")
    print(x2)
    newline()

define show_combos(l, l2):
  for (x = in_list(l)):
   then (x2 = in_list(l2)):
     print(x)
     print_string(" ")
     print(x2)
     newline()
```

## Parsed representation

The parse of a shrubbery can be represented by an S-expression:

 * Each group is represented as a list that starts `'group`, and
   the rest of the list are the elements of the group.

 * Atom elements are represented as “themselves” within a group,
   including identifers a symbols, except that an operator is
   represented as a 2-list that is `'op` followed by the operator name
   as a symbol.

 * A group sequence is represented as a list of `'group` lists.

 * An element created by `()` is represented by `'parens` consed
   onto a group-sequence list.
   
 * An element created by `[]` is represented by `'brackets` consed
   onto a group-sequence list.

 * An element created by `{}` is represented by `'braces` consed
   onto a group-sequence list.

 * A block is represented as either `'block` or `'alts` consed onto a
   group-sequence list. The representation uses `'alts` if the content
   of the block is a squence of groups started with `|`, and it's
   `'block` otherwise.

 * A block created to follow `|` appears immediately in an `'alts`
   list.

Note that a block can only appear immediately in a `'group` or `'alts`
list. Note also that there is no possibility of confusion between
symbol atoms in the input and `'group`, `'block`, etc., at the start
of a list in an S-expression representation, because symbol atoms will
always appear as non-initial items in a `'group` list.

Here are some example shrubberies with their S-expression parsed
representations:

```
define pi: 3.14

(group define pi (block (group 3.14)))
```

```
define fourth(n: integer):
  define m: n*n
  define v: m*m
  printf("~a^4 = ~a\n", n, v)
  v

(group define
       fourth
       (parens (group n (block (group integer))))
       (block
        (group define m (block (group n (op *) n)))
        (group define v (block (group m (op *) m)))
        (group printf
               (parens (group "\"~a^4 = ~a\\n\"") (group n) (group v)))
        (group v)))
```

```
if x = y
| same
| different

(group if x (op =) y (alts (block (group same))
                           (block (group different))))
```

```
define fib(n):
  match n
  | 0: 0
  | 1: 1
  | n: fib(n-1) + fib(n-2)

(group define
       fib
       (parens (group n))
       (block
        (group match
               n
               (alts
                (block (group 0 (block (group 0))))
                (block (group 1 (block (group 1))))
                (block
                 (group n
                        (block
                         (group fib
                                (parens (group n (op -) 1))
                                (op +)
                                fib
                                (parens (group n (op -) 2))))))))))))
```

## Lexeme Parsing
[lexeme-parsing]: #lexeme-parsing

The tokens used for grouping and indentation are distinct lexemes:

```
( ) [ ] { }   ; ,   : |   « »  \
```

Other lexemes are described by the grammar in the table below, where
an asterisk in the left column indicates the productions that
correspond to lexemes. Only simple forms of numbers are supported
directly (decimal integers, decimal floating point, and hexadecimal
integers, in all cases allowing `_`s between digits), but a `#{`...`}`
escape provides access to the full Racket S-expression number grammar.
Boolean literals are Racket-style, instead of reserving identifiers.
Special floating-point values similarly use a `#` notation.

Identifiers are formed from Unicode alphanumeric characters plus `_`,
where the initial character must not be a numeric character. An
identifier prefixed with `~` forms a keyword, analogous to prefixing an
identifier with `#:` in Racket.

Operators are formed from Unicode symbolic and punctuation characters
other than the ones listed above as distinct lexemes (plus a few more,
like `"` and `'`), but `|` or `:` is also
allowed in an operator name as long as it is not by itself. A
multi-character operator cannot end in `+`, `-`, or `.` to avoid
ambiguity in cases like `1+-2` (which is `1` plus `-2`, not `1` and
`2` combined with a `+-` operator), unless the operator contains
only `+`, `-`, or `.` (so `++`, `--`, and `...` are allowed).
Also, multi-character operator cannot end with `/` or contain `//` or
`/*`, because that can create ambiguities with comments.

Implicit in the grammar is the usual convention of choosing the
largest possible match at the start of a stream. Not reflected in the
grammar is a set of delimiter requirements: numbers, `#true`, and
`#false` must be followed by a delimiter. For example, `1x` is a
lexical error, because the `x` after `1` is not a delimiter.
Non-alphanumeric characters other than `_` and `.` are delimiters.
Finally, the treatment of `+` and `-` as a number prefix versus an
operator is subject to a special rule: they are parsed as operators
when immediately preceded by an alphanumeric character, `_`, `)`, `]`, or `}`
with no whitespace in between. For
example, `1+2` is `1` plus `2`, but `1 +2` is `1` followed by the
number `+2`.

When a `#{`...`}` escape describes an identifier S-expression, it is
an identifier in the same sense as a shrubbery-notation identifier.
the same holds for numbers, booleans, strings, byte strings, and
keywords. A `#{`...`}` escape must _not_ describe a pair, because
pairs are used to represent a parsed shrubbery, and allowing pairs
would create ambiguous or ill-formed representations.

A `@` starts an at-expression form similar to the notaton supported by
`#lang at-exp` (which oriented toward S-expressions and
readtable-based). The next subsection explains in more detail, but the
tabel below sketches the shape of `@` forms.

|   | nonterminal     |     |                 production                                | adjustment                     |
|---|-----------------|-----|-----------------------------------------------------------|--------------------------------|
| * | _identifier_    | is  | _alpha_ _alphanum_ *                                      |                                |
|   |                 |     |                                                           |                                |
|   | _alpha_         | is  | **an alphabetic Unicode character or** `_`                |                                |
|   |                 |     |                                                           |                                |
|   | _alphanum_      | is  | _alpha_                                                   |                                |
|   |                 | or  | **a numeric Unicode character**                           |                                |
|   |                 |     |                                                           |                                |
| * | _keyword_       | is  | `~` _identifier_                                          |                                |
|   |                 |     |                                                           |                                |
| * | _operator_      | is  | _opchar_ * _tailopchar_                                   | **not** `❘` **or** `:` ...     |
|   |                 | or  | `.` +                                                     | ... **or containing** `//` ... |
|   |                 | or  | `+` +                                                     | ... **or containing** `/*`     |
|   |                 | or  | `-` +                                                     |                                |
|   |                 |     |                                                           |                                |
|   | _opchar_        | is  | **a symbolic Unicode character not in** _special_         |                                |
|   |                 | or  | **a punctuation Unicode character not in** _special_      |                                |
|   |                 | or  | **one of ** `:`, `❘`                                      |                                |
|   |                 |     |                                                           |                                |
|   | _tailopchar_    | is  | **anything in** _opchar_ **except** `+`, `-`, `.`, `/`    |                                |
|   |                 |     |                                                           |                                |
|   | _special_       | is  | **one of** `(`, `)`, `[`, `]`, `{`, `}`, `«`, `»`         |                                |
|   |                 | or  | **one of** `"`, `;`, `,`, `~`, `#`, `\`, `_`, `@`         |                                |
|   |                 |     |                                                           |                                |
| * | _number_        | is  | _integer_                                                 |                                |
|   |                 | or  | _float_                                                   |                                |
|   |                 | or  | _hexinteger_                                              |                                |
|   |                 |     |                                                           |                                |
|   | _integer_       | is  | _sign_ ? _nonneg_                                         |                                |
|   |                 |     |                                                           |                                |
|   | _sign_          | is  | **one of** `+` **or** `-`                                 |                                |
|   |                 |     |                                                           |                                |
|   | _nonneg_        | is  | _decimal_ _usdecimal_ +                                   |                                |
|   |                 |     |                                                           |                                |
|   | _decimal_       | is  | `0` through `9`                                           |                                |
|   |                 |     |                                                           |                                |
|   | _usdecimal_     | is  | _decimal_                                                 |                                |
|   |                 | or  | `_` _decimal_                                             |                                |
|   |                 |     |                                                           |                                |
|   | _float_         | is  | _sign_ ? _nonneg_ ? `.` _nonneg_? _exp_ ?                 |                                |
|   |                 | or  | _sign_ ? _nonneg_ _exp_                                   |                                |
|   |                 | or  | `#inf`                                                    |                                |
|   |                 | or  | `#neginf`                                                 |                                |
|   |                 | or  | `#nan`                                                    |                                |
|   |                 |     |                                                           |                                |
|   | _exp_           | is  | `e` _sign_ ? _nonneg_                                     |                                |
|   |                 | or  | `E` _sign_ ? _nonneg_                                     |                                |
|   |                 |     |                                                           |                                |
|   | _hexinteger_    | is  | `0x` _hex_ _ushex_ *                                      |                                |
|   |                 |     |                                                           |                                |
|   | _hex_           | is  | **one of** `0` **through** `9`                            |                                |
|   |                 | or  | **one of** `a` **through** `f`                            |                                |
|   |                 | or  | **one of** `A` **through** `F`                            |                                |
|   |                 |     |                                                           |                                |
|   | _ushex_         | is  | _hex_                                                     |                                |
|   |                 | or  | `_` _hex_                                                 |                                |
|   |                 |     |                                                           |                                |
| * | _boolean_       | is  | `#true`                                                   |                                |
|   |                 | or  | `#false`                                                  |                                |
|   |                 |     |                                                           |                                |
| * | _string_        | is  | `"` _strelem_ * `"`                                       |                                |
|   |                 |     |                                                           |                                |
|   | _strelem_       | is  | **element in Racket string**                              | `\U` ≤ 6 digits                |
|   |                 |     |                                                           |                                |
| * | _bytestring_    | is  | `#"` _bytestrelem_ * `"`                                  |                                |
|   |                 |     |                                                           |                                |
|   | _bytestrelem_   | is  | **element in Racket byte string**                         |                                |
|   |                 |     |                                                           |                                |
| * | _sexpression_   | is  | `#{` _racket_ `}`                                         |                                |
|   |                 |     |                                                           |                                |
|   | _racket_        | is  | **any non-pair Racket S-expression**                      |                                |
|   |                 |     |                                                           |                                |
| * | _comment_       | is  | `//` _nonnlchar_                                          |                                |
|   |                 | or  | `/*` _anychar_ `*/`                                       | nesting allowed                |
|   |                 |     |                                                           |                                |
| * | _termcomment_   | is  | `#//`                                                     |                                |
|   |                 |     |                                                           |                                |
|   | _nonnlchar_     |     | **any character other than newline**                      |                                |
|   |                 |     |                                                           |                                |
| * | _atexpression_  | is  | `@` _command_ ? _arguments_ ? _body_ ?                    | no space between these parts   |
|   |                 |     |                                                           |                                |
|   | _command_       | is  | _identifier_                                              |                                |
|   |                 | or  | _keyword_                                                 |                                |
|   |                 | or  | _operator_                                                |                                |
|   |                 | or  | _number_                                                  |                                |
|   |                 | or  | _boolean_                                                 |                                |
|   |                 | or  | _string_                                                  |                                |
|   |                 | or  | _bytestring_                                              |                                |
|   |                 | or  | _racket_                                                  |                                |
|   |                 | or  | `(` _group_ * `)`                                         | usual comma-separated groups   |
|   |                 | or  | `«` _group_ `»`                                           | one spliceable group, no block |
|   |                 |     |                                                           |                                |
|   | _arguments_     | is  | `[` _group_ * `]`                                         | usual comma-separated groups   |
|   |                 |     |                                                           |                                |
|   | _body_          | is  | `{` _text_ `}`                                            | possible escapes in _text_     |
|   |                 | or  | _atopen_ _text_ _atclose_                                 | _atcloser_ matching _atopen_   |
|   |                 |     |                                                           |                                |
|   |  _atopen_       | is  | `❘` _asciisym_ * `{`                                      |                                |
|   |                 |     |                                                           |                                |
|   |  _atclose_      | is  | `}` _asciisym_ * `❘`                                      | reverses and flips paren-like  |

# At-notation using `@`

An `@` form of the shape

```
 @«command ...»[arg, ...]{ body }
```

is parsed into the same representation as

```
 command ...(arg, ..., [parsed_body, ...])
```

That is, the command part is left at the front and spliced into its
enclosing group, while the argument and body parts are wrapped with
parentheses to make them like arguments. The body text is parsed into
a list of string literals and escapes.

The command part usually does not have `«»`, and it is instead
usually written as an identifier, operator, or parenthesized term. The
argument and body parts, when present, always use `[]` and `{}`,
respectively. Any of the three parts can be omitted, but when
mulltiple parts are present, they must have no space between them or
the leading `@`. When the argument and body parts are both
omitted, the command part is simply spliced into its context.

The conversion to a call-like form and keeping the body in a separate
list are the two main ways that shrubbery `@` notation differs from
`#lang at-exp` notation. The other differences are the use of
`«`...`»` instead of `|`...`|` for delimiting a command, and the use
of `@//` instead of `@;` for comments. The details are otherwise meant
to be the same, and the rest of this section is mostly a recap.

The body part is treated as literal text, except where `@` is used in
the body to escape. An unescaped `}` closes the body, except that an
unescaped `{` must be balanced by an unescaped `}`, with both treated
as part of the body text. Instead of `{`, the body-starting opener can
be `|` plus `{` with any number of ASCII punctuation and symbol
characters (other than `{`) in between; the corresponding closer is
then the same sequence in reverse, except that some characters are
flpped: `{` to `}`, `(` to `)`, `)` to `(`, `[` to `]`, `]` to `[`,
`<` to `>`, and `>` to `<`. With an `|`...`{` opener, an escape is
formed by using the opener followed by `@`, while opener–closer pairs
balance within the body text. The parsed form of the body breaks up
the body text into lines and `"\n"` as separate string literals in the
parsed list form, with each escape also being its own element in the
list form. Parsed body text also has leading and trailing whitespace
adjusted the same as with `#lang at-exp`.

After the `@` of an escape in body text, the escape has the same
form as an at-notaton form that starts with `@` as a shubbery. That
is, `@` forms are essentially the same whether starting in shrubbery
mode or body-text mode.

In body text, there are two additional comment forms that are not
supported in shrubbery mode. A `@//{` starts a block comment that ends
with `}`, and the comment form is not part of the body text. The `@//`
comment form must be prefixed with an opener when its enclosing body
is started with an opener that isn't just `{`, and the `{` after `@//`
can more generally be an `|`...`{` opener with the corresponding
closer. Opener–closer pairs must be balanced in the commented block,
the same as in body text. A `@//` comment form (prefixed with an
opener as needed to form an escape) that is not followed by `{` or an
`|`...`{` opener comments out the rest of the line, including a
comment-terminating newline.

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

See [parse.rkt](parse.rkt). Note that if you run with no arguments,
the that program will read from stdin. Supply one or more files to
read from those files instead of stdin. Supply `--recover` to continue
parsing after indentation or _closer_ errors.

See [demo.shrb](demo.shrb), [interp.shrb](interp.shrb), and
[weird.shrb](weird.shrb) for more examples.

# Drawbacks
[drawbacks]: #drawbacks

Shrubbery notation may not be a good choice where precise and complete
grouping is needed, both because its grouping is coarse-grained and
the grouping rules generate lots of extra `group` layers.

Shrubbery notation does not resolve the question of how infix
expressions parse. There is no precedence at the shrubbery level, for
example, other than the way that a `:` has higher precedence (in a
sense) than `|`.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The lexeme-level syntax is chosen to be familiar to programmers
generally. The sequence `1+2` is one plus two, not a strangely spelled
identifier. Tokens like `(`, `,`, `{` and `;` are used in familiar
ways. Shrubbery notation provides enough grouping structure that code
navigation and transformation should be useful and straightforward in an
editor.

Parentheses in shrubbery notation do not disable indentation, unlike
some indentation-sensitive notations. That choice supports a language
in shrubbery notation where parentheses can be added around any
expression — even if the expression is written with indentation
(although the expression may need to be shifted right to preserve
relative indentation, depending on how parentheses are added).

The inclusion of `|` in shrubbery notation reflects the fact that
conditional forms (such a `if`, `cond`, and `match`) are important and
common. A distinct, pleasant, and uniform pattern for conditionals
deserves direct support in the notation.

Requiring a preceding `:` or preceding/following `|` for
block-creating indentation is mostly a kind of consistency check to
enable better and earlier errors when indentation goes wrong. It also
allows indentation that starts with an operator to continue a group;
it's possible for bad indentation to inadvertently cause an operator
to be treated as continuing a group, but hopefully that will be rare.
Always requiring a preceding `:` before an indented `|` line would be
consistent, but it adds extras `:`s where `|` already provides one
consistency check. Allowing an optional `:` before `|` would work, but
programmers may then choose differently on omitting or including the
`:`, leading to subtly divergent conventions.

Explicit block grouping via `«` and `»` is expected to be rare. The
grouping characters were intentionally chosen from the Latin-1
extension of ASCII to avoid reserving additional ASCII characters.

Making whitespace and comment lines ignored in all contexts means that
they can be freely added without intefering with grouping. The `\`
continuation operator is somewhat unusual in that it skips blank and
comment lines to continue, as opposed to requiring `\` on every
continuing line; that, too, allows extra blank and comment lines to be
added, even amid continuing lines.

The interaction of indentation and `\` differs slightly from Python,
which does not count the space for `\` itself or any leading
whitespace on a continuing line toward indentation. Counting the
leading whitespace on a continuing line has the advantage that it can
reach an arbitrary amount of identation within a constrained textual
width. Counting the `\` itself is consistent with ignoring `\` when it
appears within a line, so grouping stays the same whether there's a
newline or the continue line immediately after `\`. The whitespace
role of `\` also means that spaces can be turned into `\` to “harden”
code for transfer via media (such as email) that might mangle
consecutive spaces.

Using `~` for keywords has a precedent in OCaml. Using `~` for
keywords uses up a character that might otherwise be used for
operators, but keywords seem useful enough to be worth this cost. The
notion of keywords as distinct from identifiers has been liberating
for Racket syntax (particularly since keywords can be kept disintinct
from expressions more generally), and we expect similar benefits for
having keywords in shrubbery notation.

The `#{....}` escape to S-expressions bridges between shrubbery
notation and Racket identifiers. For example, `#{exact-integer?}` is
an identifier with `-` and `?` as part of the identifier. Shrubbery
notation could be adapted to support Lisp-style identifiers by
requiring more space around operators, but the rule for continuing a
group between `(` and `)` or `[` and `]` currently depends on
distinguishing operators from non-operators.

For `@`, the choice of treating `@f[arg]{text}` as `f(arg, ["text"])`
instead of `f(arg, "text")` reflects experience with S-expression `@`
notation. Although it seems convenient that, say `@bold{x}` is treated
as `(bold "x")`, the consequence is that a function like `bold` might
be implemented at first to take a single argument; later, a use like
`@bold{Hello @name}` breaks, because two arguments are provided.
Making explicit the list that's inherent in body parsing should help
reduce such mistakes (or bad design choices) for functions that are
meant to be used with `@` notation.

# Prior art
[prior-art]: #prior-art

Indentation-sensitive parsing and the use of `:` is obviously informed
by Python.

Sampling notation's rules relating indentation, lines, `;`, and `:`
are originally based on the [`#lang
something`](https://github.com/tonyg/racket-something) reader, which
also targets an underlying expander that further groups tokens.
Shrubbery notation evolved away from using `{}` for blocks, however,
because `:` was nearly always preferred in experiements with the
notation. For the very rare case that explicit gropuing is needed for
a block, `«` and `»` can be used. Freeing `{}` from use for blocks,
meanwhile, allows its use for set and map notations.

Shrubbery notation is also based on
[Lexprs](https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/lexpr/0004-lexpr.md),
particularly its use of `|`. Lexprs uses mandatory `:` and `|` tokens
as a prefix for indentation, and it absorbs an additional line after
an indented section to allow further chaining of the group. Although
`«»` can be used to form multiple subgroups within a shrubbery group,
the notation discourages that style in favor of further nesting (or,
in the case of `if`, in favor of `|` notation like other
conditionals).

Shrubbery notation is in some sense a follow-up to [sapling
notation](https://github.com/mflatt/racket2-rfcs/blob/sapling/sapling/0005-sapling.md).
The primary difference is that shrubbery notation is
indentation-sensitive, while sapling notation is
indentation-insensitive. Indentation sensitivity and block conventions
in shrubbery notation avoid some delimiters and blank lines that are
needed in sapling notation.

More generally, shrubbery notation takes inspiration from
S-expressions and alternative S-expression notations. The idea that,
even in an S-expression-like setting, some parsing can be deferred a
later parser has many precedents, including Clojure's choice of where
to put parentheses and notations that use something like `$` to escape
to infix mode.

# Unresolved questions
[unresolved-questions]: #unresolved-questions


# Future possibilities
[future-possibilities]: #future-possibilities

Like other notation designs, this one leaves open exactly the way that
the notation would be used to express a new programming language. The
examples are meant to be suggestive and have influenced many of the
notational choices, though.
