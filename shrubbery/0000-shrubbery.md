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

Here are some example shrubberies. A `:` in the middle of a line is the
same as starting a new line with indentation for the part after the
`:`. (Extra `:`s are allowed, but they are non-standard, and we don't
use any extra `:`s here.)

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
(via indentation or `:`) and conditional branches (via `|`).

Identifiers are C-style with alphanumerics and underscores. Operators
are sequences of symbolic characters in the sense of `char-symbolic?`,
roughly. Numbers are written in some reasonable way distinct from
identifiers. No spaces are needed between operators and non-operators,
so `1+2` and `1 + 2` mean the same thing. Comments are C-style.

The following tokens are used for grouping, in addition to line breaks
and indentation:

```
( ) [ ] { }   ; ,   : |   \
```

Parentheses, square brackets, and curly braces are used to form groups
in the obvious way. A `;` or `,` acts as a group separator, even
within a single line. A `:` or `|` treats remaining item on the same
line like a new indented line, which forms a subgroup. A `\` continues a
group across a line break.

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
is disallowed if it would create an empty group, except that a traiing
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

Identation is allowed only on a line where the previous line ends with
`:`, when the previous line ends with `|`, or the indented line starts
with `|`.

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
This rule is consistent with `:` as a knid of redundant indicator for
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

The correspondence among blocks created `:`, indentation, and `{}`
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

## Continuing a group with `\`

As a last resort, `\` can be used at the end of a line to continue the
group on the next line as it if were one line; the `\` does not appear
in the parsed form. A `\` anywhere else in a line is ignored, except
that a `\` as the first non-whitespace entry on a non-continuing line
determines the line's indentation.

```
this is \
  the first group
this \ is \ the \ second \ group
\ this is the third group
```

## More examples

Here are more example shrubberies.

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

 * Atom elements are represented as “themselves” within a group.

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
        (group define m (block (group n * n)))
        (group define v (block (group m * m)))
        (group printf
               (parens (group "\"~a^4 = ~a\\n\"") (group n) (group v)))
        (group v)))
```

```
if x = y
 | same
 | different

(group if x = y (alts (block (group same))
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
                                (parens (group n - 1))
                                +
                                fib
                                (parens (group n - 2))))))))))))
```


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

The lexeme-level syntax here would require some sort of bridge to
Racket names that don’t fit C-style syntax.

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

A full shrubbery-notation design should incorporate `at-exp` notation,
too (where `@` escapes return to shrubbery notation instead of
S-expressions).

To bridge to Racket identifiers, something like `#{....}` could be the
syntax for an identifier that has any character between the curly
braces (with some suitable generalization to accommodate `{` and `}` in
identifier names), so `#{exact-integer?}` would be an identifier with
`-` and `?` as part of the identifier.

Shrubbery notation could be adapted to support Lisp-style identifiers by
requiring more space around operators, but the rule for continuing a
group between `(` and `)` or `[` and `]` currently depends on
distinguishing operators from non-operators.

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
