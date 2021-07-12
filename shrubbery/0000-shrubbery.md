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
define identity(x): x

define fib(n):
  cond
   | n == 0: 0
   | n == 1: 1
   | else: fib(n-1) + fib(n-2)

define print_sexp(v):
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

Forms like `define`, `cond`, and `match` are not specified by
shrubbery notation, since specifying those forms is up to a language
that is built on top of shrubbery notation. Still, shrubbery notation
is meant to accommodate a particular kind of syntax for nested blocks
(via indentation and/or `:`) and conditional branches (via `|`).

Identifiers are C-style with alphanumerics and underscores. Operators
are sequences of symbolic characters in the sense of `char-symbolic?`,
roughly. No spaces are needed between operators and non-operators, so
`1+2` and `1 + 2` mean the same thing. Comments are C-style. See
[lexeme parsing](#lexeme-parsing) for more information.

The following tokens are used for grouping, in addition to line breaks
and indentation:

```
( ) [ ] { }   ; ,   : |   \
```

Parentheses, square brackets, and curly braces are used to form groups
in the obvious way. A `;` or `,` acts as a group separator, even
within a single line. A `:` or `|` treats remaining item on the same
line like a new indented line, which forms a subgroup. A `\` continues
a line, effectively shifting all columns on the next line as if they
appeared immediately after the `\`.

## Grouping by lines

The main grouping rule is that sequences on different lines with the
same indentation create separate groups, one for each line.

```
this is the first group
this is the second group
```

Comments and lines with only whitespace are ignored. They don't count
when this document says “the previous line” or “the next line.”

## Grouping by _opener_-_closer_, including blocks

An _opener_-_closer_ pair `(` and `)`, `[` and `]`, or `{` and `}`
forms a nested group that can span lines. Within the _opener_-_closer_
pair, separate lines at the same indentation form separate groups.
Within `()` or `[]`, groups on separate lines must also be separated
by `,`.

```
group 1
[group 2 - subgroup one,
 group 2 - subgroup two,
 (group 2 - subgroup three - subsubgroup A,
  group 2 - subgroup three - subsubgroup B)]
group 3
group 4 {has a subgroup
         and another subgroup}
```

Parsing retains whether a subgroup is formed by `()`, `[]`, or `{}`. A
subgroup formed by `{` and `}` is also known as a _block_. In the
above example, the fourth group has three elements: the identifier
`group`, the number `4`, and a block that has two groups (each with
three identifiers).

As a special rule, when a group is immediately within a `()` or `[]`
group sequence, the group continues to the next line when that next
line starts with an operator. This rule allows long expressions with
infix operators to be broken across lines more naturally and readably
than using `\`.

```
(my_favorite_number
 + your_favorite_number
 + their_favorite_numbers)
```

## Group separators `;` and `,`

A `;` or a `,` separates two groups on the same line. A `;` is allowed
in any context except between groups immediately within `()` or `[]`.
A `,` is allowed only to separate groups of an immediate sequence
within `()` or `[]`, and `,` is also required to separate groups
witin `()` and `[]`.

The following two groups are the same:

```
[(subgroup one, subgroup two,
  subgroup three),
 {subgroup X; subgroup Y}]

[(subgroup one,
  subgroup two,
  subgroup three),
 {subgroup X
  subgroup Y}]
```

These following forms are disllowed, because they use the wrong
separator or because a required separator is missing.


```
// Not allowed
(1; 2)
[1; 2]
{1, 2}

// Not allowed
(1
 2)
[1
 2]
```

The `;` and `,` separators interact differently with subgroups formed
by indentation, `:`, and `|`. A `,` closes subgroups as necessary to
reach an enclosing `()` or `[]`, while a `;` separate groups within a
nested group sequence. A `;` will never create an empty group. A `,`
is disallowed if it would create an empty group, except that a trailing
`,` is allowed.

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

## Grouping by indentation

A sequence of groups has a particular indentation that is determined
by the first group in the sequence. Subsequent groups in a sequence
must start with the same indentation as the first group.

Indentation relative to the current group is equivalent to wrapping
the indented lines with `{` and `}`. That is, indentation creates a
block. The inserted `{` is placed on the line just before the indented
group, and the `}` is placed just before the line where indentation
becomes shallower again. All four of the following groups are the
same, each with one block that has two nested groups:

```
hello:
  world
  universe

hello:
       world
       universe

hello { world
        universe }

hello { world ; universe }
```

As illustrated in those examples, there is no constraint on how much
indentation a nested group sequence must use, as long as the indentation
is more than the enclosing group. Within an _opener_-_closer_ pair, a
nested group sequence can start at any indentation; it doesn't have to be
indented to the right of the _opener_.

```
hello {
  world
  universe
}
```

Indentation to start a group is allowed only on a line where the
previous line ends with `:` or the indented line starts with `|`.

```
// Not allowed
hello
  world
  universe
```

## Grouping by `:`

A `:` in the middle of a line is equivalent to starting a new line
after the `:` and indenting up to the position of the token following
the `:`, which means that it creates a block. The following group is
also equivalent to the above groups:

```
hello: world
       universe
```

As a special rule, if the block that would be created by `:` has one
group that is itself a block, then `:` does not create a new block.
This rule is consistent with `:` as a kind of redundant indicator for
indentation, and it generalizes by allowing an optional `:` before an
explicit `{`. In fact, any number of optional `:`s can appear. The
following groups are the same as the earlier examples:

```
hello: {world; universe}

hello: : : : : world
               universe
```

This special rule for `:` means that you don't have to worry about
whether a `:` is redundant or whether it creates some subtle or
important extra layer of blocking, and the parser will not be
needlessly pendantic when you're writing or revising code. However,
the standard style, which might be enforced with a code-formatting
tool, is to omit any `:` that is not required.

Note that if a `:` appears at the end of a group, then it forces an
empty block, which means that a `:` doesn't just disappear if there's
no content after it. For example `(void:)` is the same as `(void {})`,
not `(void)`.

The correspondence among blocks created by `:`, indentation, and `{}`
means that a programmer can choose between single-line forms using
`{}` and `;` or multi-line forms using indentation.

## Grouping by `|`

A `|` is implicitly shifted half a column right (so, implicitly nested),
and it is implicitly followed by a `:` that conceptually occupies same
column as the `|`. A `|` that is not at the start of a group is also
implicitly *preceded* by a `:`. Note that the implicit `:` following a
`|` is subject to the special rules for `:`, which is that it doesn't
create a new block if one is already created (by indentation or `{`),
and it can force an empty block.

```
hello
 | world
 | universe

hello
| world
| universe

hello: | world
       | universe

hello | world
      | universe

hello |
        world
      |
        universe

hello | { world }
      | { universe }

hello { | { world }
        | { universe } }
```

A `|` has its own special rule: if a `|` appears on the same line as a
`|` and would start a block immediately within that `|`'s block, then
`|` instead terminates the previous `|`'s block and continues its
enclosing group sequence with a new `|` group that is *not* implicitly
preceded by a `:`. The intent and consequence of this rule is that
multiple `|`s can be used on a single line as an alternative to
starting each `|` on its own line, making the following groups the
same as the above groups:

```
hello | world | universe

hello: | world | universe

hello
 | world | universe

hello | { world } | { universe }

hello { | world | universe }

hello { | { world } | { universe } }

hello { | { world } ; | { universe } }
```

The implicit shifting of `|` by half a column is consistent with its
visual representation, and it avoids the possibility of a group
sequence that contains a mixture of `|`-started groups and other kinds
of groups. Nevertheless, standard indentation includes a one additional
space of indentation before `|`.

## Continuing a line with `\`

As a last resort, `\` can be used at the end of a line (optionally
followed by whitespace and coments on the line) to continue the next
line as it if were one line continuing with the next line. The itself
`\` does not appear in the parsed form. A that is not at the end of a
line (followed by whitespace and coments) is treated the same as
whitespace.

Lines contianing only whitespace and comments do not count as “the
next line” even for `\` continuations, so any number of whitespace of
comment lines in the next can appear between `\` and the line that it
continues.


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

 * A block is represented as either `'block` or `'alts` consed onto a
   group-sequence list. The representation uses `'alts` if the content
   of the block is a squence of groups started with `|`, and it's
   `'block` otherwise.

 * An element created by `()` is represented by `'parens` consed
   onto a group-sequence list.
   
 * An element created by `[]` is represented by `'braces` consed
   onto a group-sequence list.

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
( ) [ ] { }   ; ,   : |   \
```

Other lexemes are described by the grammar in the table below, where
an asterisk in the left column indicates the productions that
correspond to lexemes. Only simple forms of numbers are supported
directly (decimal integers, decimal floating point, and hexadecimal
integers, in all cases allowing `_`s between digits), but a `#{`...`}`
escape provides access to the full Racket S-expression number grammar.
Boolean literals are Racket-style, instead of reserving identifiers.
Special floating-point values similarly use a `#` notation.

Operators are formed from Unicode symbolic and punctuation characters
other than the ones listed above as distinct lexemes (plus a few more,
like `"`), but `|` or `:` is also
allowed in an operator name as long as it is not by itself. A
multi-character operator cannot end in `+`, `-`, or `.` to avoid
ambiguity in cases like `1+-2` (which is `1` plus `-2`, not `1` and
`2` combined with a `+-` operator).

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
the same holds for numbers, booleans, strings, and byte strings. A
`#{`...`}` escape must _not_ describe a pair, because pairs are used
to represent a parsed shrubbery, and allowing pairs would create
ambiguous or ill-formed representations.



|   | nonterminal     |     |                 production                                | adjustment             |
|---|-----------------|-----|-----------------------------------------------------------|------------------------|
| * | _identifier_    | is  | _alpha_ _alphanum_ *                                      |                        |
|   |                 |     |                                                           |                        |
|   | _alpha_         | is  | **an alphabetic Unicode character or** `_`                |                        |
|   |                 |     |                                                           |                        |
|   | _alphanum_      | is  | _alpha_                                                   |                        |
|   |                 | or  | **a numeric Unicode character**                           |                        |
|   |                 |     |                                                           |                        |
| * | _operator_      | is  | _opchar_ * _tailopchar_                                   | **not** `|` **or** `:` |
|   |                 |     |                                                           |                        |
|   | _opchar_        | is  | **a symbolic Unicode character**                          |                        |
|   |                 | or  | **a punctuation Unicode character not in** _special_      |                        |
|   |                 |     |                                                           |                        |
|   | _tailopchar_    | is  | **anything in** _opchar_ **except** `+`, `-`, `.`         |                        |
|   |                 |     |                                                           |                        |
|   | _special_       | is  | **one of** `(`, `)`, `[`, `]`, `{`, `}`, `"`              |                        |
|   |                 | or  | **one of** `;`, `,`, `#`, `\`, `_`, `@`                   |                        |
|   |                 |     |                                                           |                        |
| * | _number_        | is  | _integer_                                                 |                        |
|   |                 | or  | _float_                                                   |                        |
|   |                 | or  | _hexinteger_                                              |                        |
|   |                 |     |                                                           |                        |
|   | _integer_       | is  | _sign_ ? _nonneg_                                         |                        |
|   |                 |     |                                                           |                        |
|   | _sign_          | is  | **one of** `+` **or** `-`                                 |                        |
|   |                 |     |                                                           |                        |
|   | _nonneg_        | is  | _decimal_ _usdecimal_ +                                   |                        |
|   |                 |     |                                                           |                        |
|   | _decimal_       | is  | `0` through `9`                                           |                        |
|   |                 |     |                                                           |                        |
|   | _usdecimal_     | is  | _decimal_                                                 |                        |
|   |                 | or  | `_` _decimal_                                             |                        |
|   |                 |     |                                                           |                        |
|   | _float_         | is  | _sign_ ? _nonneg_ ? `.` _nonneg_? _exp_ ?                 |                        |
|   |                 | or  | _sign_ ? _nonneg_ _exp_                                   |                        |
|   |                 | or  | `#inf`                                                    |                        |
|   |                 | or  | `#neginf`                                                 |                        |
|   |                 | or  | `#nan`                                                    |                        |
|   |                 |     |                                                           |                        |
|   | _exp_           | is  | `e` _sign_ ? _nonneg_                                     |                        |
|   |                 | or  | `E` _sign_ ? _nonneg_                                     |                        |
|   |                 |     |                                                           |                        |
|   | _hexinteger_    | is  | `0x` _hex_ _ushex_ *                                      |                        |
|   |                 |     |                                                           |                        |
|   | _hex_           | is  | **one of** `0` **through** `9`                            |                        |
|   |                 | or  | **one of** `a` **through** `f`                            |                        |
|   |                 | or  | **one of** `A` **through** `F`                            |                        |
|   |                 |     |                                                           |                        |
|   | _ushex_         | is  | _hex_                                                     |                        |
|   |                 | or  | `_` _hex_                                                 |                        |
|   |                 |     |                                                           |                        |
| * | _boolean_       | is  | `#true`                                                   |                        |
|   |                 | or  | `#false`                                                  |                        |
|   |                 |     |                                                           |                        |
| * | _string_        | is  | `"` _strelem_ * `"`                                       |                        |
|   |                 |     |                                                           |                        |
|   | _strelem_       | is  | **element in Racket string**                              | `\U` ≤ 6 digits        |
|   |                 |     |                                                           |                        |
| * | _bytestring_    | is  | `#"` _bytestrelem_ * `"`                                  |                        |
|   |                 |     |                                                           |                        |
|   | _bytestrelem_   | is  | **element in Racket byte string**                         |                        |
|   |                 |     |                                                           |                        |
| * | _sexpression_   | is  | `#{` _racket_ `}`                                         |                        |
|   |                 |     |                                                           |                        |
|   | _racket_        | is  | **any non-pair Racket S-expression**                      |                        |
|   |                 |     |                                                           |                        |
| * | _comment_       | is  | `//` _nonnlchar_                                          |                        |
|   |                 | or  | `/*` _anychar_ `*/`                                       | nesting allowed        |
|   |                 |     |                                                           |                        |
|   | _nonnlchar_     |     | **any character other than newline**                      |                        |


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

The parsing rules for shrubbery notation are somewhat complex. There
are several special cases for `:` and `|`, for example, that make the
parser implementation irregular. This complexity is arguably
deceptive, however, because it serves to reduce rather than increase
differentiation in parsed output, which is intended to make the
notation easier for humans to read and write.

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

Shrubbery notation would be consistent and without ambiguity if we
drop the requrement to precede indentation with `:` or `|`. Requiring
a preceding `:` or `|` is a kind of consistency check to enable better
and earlier errors when indentation goes wrong.

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

The `#{....}` escape to S-expressions bridges to Racket identifiers.
For example, `#{exact-integer?}` would be an identifier with `-` and
`?` as part of the identifier. Shrubbery notation could be adapted to
support Lisp-style identifiers by requiring more space around
operators, but the rule for continuing a group between `(` and `)` or
`[` and `]` currently depends on distinguishing operators from
non-operators.

The `@` is reserved for a future extension to `at-exp` notation
adapted suitably to srhubbery notation.

# Prior art
[prior-art]: #prior-art

Indentation-sensitive parsing and the use of `:` is obviously informed
by Python.

Sampling notation's rules relating indentation, lines, `{}`, `;`, and
`:` are taken from the [`#lang
something`](https://github.com/tonyg/racket-something) reader, which
also targets an underlying expander that further groups tokens.
Shrubbery notation departs from `#lang something` conventions in a few
ways, such as ignoring more `:`s and preserving indentation in `()`, and
it adds the treatment of `,` and `|`.

Shrubbery notation is also based on
[Lexprs](https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/lexpr/0004-lexpr.md),
particularly its use of `|`. Lexprs uses mandatory `:` and `|` tokens
as a prefix for indentation, and it absorbs an additional line after
an indented section to allow further chaining of the group. Although
`{}` can be used to form multiple subgroups within a shrubbery group,
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
