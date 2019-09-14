- Feature Name: Sapling notation
- Start Date: 2019-09-04
- RFC PR: [racket/racket2-rfcs#119](https://github.com/racket/racket2-rfcs/pull/119)

# Summary
[summary]: #summary

Sapling notation is similar to S-expression notation, but instead of
generating fully formed trees, it is intended to partially group input
for further enforestation by another parser (e.g., as in Honu). The
notation is line-sensitive but indentation-insensitive. The identation
of a sapling can be normalized using only lexemes without further
parsing information. The parsed form of a sapling imposes grouping to
ensure that further parsing is consistent with the sapling's normal
indentation.

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
breaks and indentation to impart grouping information.
Indentation-sensitive parsing comes with its own drawbacks, however;
those drawbacks can be magnified (e.g., forcing programmers to add
line breaks or otherwise restructure code) if we continue to insist
that all grouping is represented at the lexeme level.

Sapling notation explores a point in the design space where the
notation is

 - line-sensitive but not indentation sensitive, and
 - intended to constrain grouping but not reflect every detail of grouping.

Line sensivity has some of the same problems as indentation
sensitivity, but the problems are much fewer in practice. After all,
unless your Lisp/Scheme/Racket code has no line comments, it is
already in a line-sensitive notation.

Defering complete grouping to another parser, meanwhile, relieves a
burden on the notation to pin down every grouping detail, so the
notation can be much less insistent about where line break are
required. At the same time, line-based grouping can constrain parsing
to ensure that line breaks and indentation in the source are not
misleading.

Sampling notation is inspired by
[Lexprs](https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/text/0004-lexpr.md)
and the idea that certain operators at the start and end of a line
indicate how lexemes are grouped and where groups start and continue.


# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Here are some example saplings in standard indentation.

```
define pi = 3.14

define fib(n):
  log_error("fib called")
  cond | (n = 0): 0
       | (n = 1): 1
       | else: fib(n-1) + fib(n-2)

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
  match n
  | 0: 0
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
  define m = n*n
  define v = m*m
  printf("~a^4 = ~a\n", n, v)
  v

struct posn(x, y)
& property prop_equal_and_hash \
  (let (hc = (lambda (a: posn, hc):
                hc(a.x) + hc(a.y))):
     [lambda (a: posn, b: posn, eql):
        (eql(a.x, b.x)
         && eql(a.y, b.y)),
      hc,
      hc])

define go(): {
  define helper(n):
    list(n, n)

  define more(m):
    if (m == 0)
    | "done"
    | more(m - 1)

  helper(more(9))
}

define curried = {
  lambda (x):
    lambda (y):
      lambda (z):
        list(x, y, z)
}

let (x = 1,
     y = 2):
  printf("About to add")
  x+y

define show_zip(l, l2):
  for (x = in_list(l),
       x2 = in_list(l2)):
    print(x)
    print_string(" ")
    print(y)
    newline()
```

Identifiers are Java-style with alphanumerics and underscores.
Operators are sequences of symbolic characters in the sense of
`char-symbolic?`, roughly. Some operators, including `|` and `:`, are
treated specially for indentation and grouping, but that doesn't
preclude their use like other operators. Numbers are written in some
reasonable way distinct from identifiers. No spaces are needed between
operators and non-operators, so `1+2` and `1 + 2` mean the same thing.
Function calls, recognized as having no white space between an expression
and open parenthesis, are distinct from other forms at the reader
level.

### Grouping overview

 - The default rule is that every line starrts a new group. A file
   is a sequence of groups.

 - Pairs of _opener_ and _closer_, such as `(` and `)`, contain a
   sequence of groups that counts as a single entity in the enclosing
   group. So, _opener_-_closer_ pairs are one way to have newlines
   within the group outsde of the pair, although the newlines separate
   indvidual groups between within the pair.

   ```
     group one
     (subgroup two.a
      subgroup two.b)
     group three
   ```

 - The simplest ways to continue a group in a new line are `&` at the
   start of a line (after any whitespace/comments) or `\` at the end
   of the line (before whitespace/comments). These aren't the most
   common ways to continue a group, though.

   ```
     This group became \
       way too long \
       to put on a single line

     struct posn(x, y)
     & mutable
     & transparent
   ```

 - The most common way to continue a group across a line is with `:`.
   A `:` at line boundary (either the end of one line or the start of
   the next) continues the group. But `:` has a second job, which is
   that it starts a nested sequence of groups, much in the same way
   that _opener_-_closer_ pairs create nested groups. There's no
   explicit closer to go with `:`.

   ```
   define sqr(x):
      x*x
   ```

 - A blank line terminates all active groups up to an enclosing
   _opener_-_closer_ pair. A `,` or `;` has the same effect. So, a
   blank line is a common way to end a `:` subgroup, a `,` is a
   common way to end a `:` subgroup between `(` and `)`, and a `;` is
   a common way to end a `:` subgroup between `{` and `}`.

   ```
   define dist(x, y):
      sqrt(x*x + y*y)

   define also_dist(x, y): {
      define sqr(x):
        x*x

      sqrt(sqr(x) + sqr(y))
   }

   define compact_dist(x, y): {
      define sqr(x):
        x*x;
      sqrt(sqr(x) + sqr(y))
   }
   ```

 - A `|` at the start of a line continues a group and starts subgroups
   in the same way as `:`. However, a `|` also closes active
   subgroups up to a preceding `|` (within the same _opener_-_closer_
   pair). This property makes `|` a kind of alternative to using
   `,`-separated groups between parentheses. A blank line, `,`, or `;`
   meanwhile closes all active subgroups without stopping at a `|`.
   Overall, a blank line, `,`, or `;` is effectively stronger as a
   terminator than a `|`, but `|` is still strong enough to terminate
   a `:`.

   ```
   define make_multiplier(n):
      match n
      | 0: lambda (m): 0
      | 1: lambda (m): m
      | -1: lambda (m): -m
      | n: lambda (m):
             log_warning("slowest case")
             m*n
   ```

 - In the special case of a group immediately within `(` and `)` or
   `[` and `]`, any operator is allow as a line-continuing starter.
   So, while `X + Y` cannot be split across lines—except by resorting
   to a `\`— the form `(X + Y)` can be split across lines by adding a
   newline before `+`, which is handy if `X` and `Y` stand for large
   expressions.

   ```
    lambda(first_arg, second_arg):
       sqrt(first_arg * first_arg
            + second_arg * second_arg)
   ```

 - The parsed form of `(` and `)` or `[` and `]` records the use of
   parentheses or square brackets. The parsed form of `{` and `}`, in
   contrast, just forms a group of groups. Similarly, the parsed form
   of `,` records the comma as part of the enclosing group, while `;`
   silently terminates a group. A `\` silently continues a group on
   the next line, while `:` is recorded as part of the group that it
   continues.

Here are some saplings each followed by the corresponding parsed forms
as represented by an S-expression:

```
define pi = 3.14

(#%all (#%grp define pi = 3.14))

define
| fib(0): 0
| fib(1): 1
| fib(n): fib(n-1) + fib(n-2)

(#%all
 (#%grp
  define
  \|
  (#%grp (#%call fib (#%grp 0)) => 0)
  \|
  (#%grp (#%call fib (#%grp 1)) => 1)
  \|
  (#%grp
   (#%call fib (#%grp n))
   :
   (#%call fib (#%grp n - 1))
   +
   (#%call fib (#%grp n - 2)))))

define show_combos(l, l2):
  for (x => in_list(l))
  &   (x2 => in_list(l2)):
    printf("<~a, ~a>\n", x, x2)

(#%all
 (#%grp
  define
  show_combos
  (#%paren (#%grp l) |,| (#%grp l2))
  :
  (#%grp
   (#%grp
    for
    (#%paren (#%grp x => (#%grp (#%grp in_list (#%paren (#%grp l))))))
    &
    (#%paren (#%grp x2 => (#%grp (#%grp in_list (#%paren (#%grp l2))))))
    :
    (#%grp
     (#%grp
      printf
      (#%paren (#%grp "\"<~a, ~a>\\n\"") |,| (#%grp x) |,| (#%grp x2))))))))
```

In a Racket implementation of a language based on saplings, the intent
is that `#%grp` acts like `begin` on expression and definition, where you
can always wrap more of them.

### Indentation overview

The standard identation of saplings starts each new group within a
group sequence at the same column. The start of nested groups depends
on how it is created:

 * `(` and `[` indent nested subgroups to line up after the _opener_.

 * `|` indents nested subgroups to one space after the `|`.

 * `:` as a line-starter or line-middle indents subgroups to one space
   after the `:`.

 * `:` as a line-ender indents subgroups to one step larger than the
   current group's indentation.

 * `{` as a line-starter or line-middle indents subgroups like `(` or `[`.

 * `{` as a line-ender indents subgroups like `:` as a line ender, unless
   it is immediately preceded by a `:`.

 * `: {` as a line-ender indents like line a line-ending `{` or `:`.
   (The `:` and `{` in `: {` can be separate by whitespace and
   comments.)

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

See [parse.rkt](parse.rkt) and [indent.rkt](indent.rkt). Note that if
you run with no arguments, the that program will read from stdin,
which is useful for checking your expectations against the parser or
indenter. Supply one or more files to read from those files
instead of stdin.

See [demo.sap](demo.sap), [interp.sap](interp.sap), and
[weird.sap](weird.sap) for more examples.

# Drawbacks
[drawbacks]: #drawbacks

Sapling notation is not a good choice where precise and complete
grouping is needed, both because its grouping is coarse-grained and
the grouping rules generate lots of extra `#%grp` layers.

Saplings do not resolve the question of how infix expressions parse.
There is no precedence at the sapling level, for example, other than
the way that an `:` has a higher precdence than a `|`.

The lexeme-level syntax here would require some sort of bridge to
Racket names that don't fit that syntax.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The lexeme-level syntax is chosen to be familiar and to distniguish
function calls from other kinds of syntactic forms. The sequence `1+2`
is one plus two, not a strangely spelled identifier. Extra
parenthesese are always allowed (pending the sampling consumer's
cooperation) and can be freely used to disambiguate grouping. Tokens
like `(`, `,`, `{` and `;` are used in familar ways. Saplings provide
enough grouping structure that code-navigaton and -transformation
should be useful and straighforward in an editor. You can select a
range of code to reindent if editing has made it unreadable.

There may be room to tweak the grouping rules to avoid unnecessary
`#%grp`s.

Sapling notation could be adapted to support Lisp-style identifiers by
requiring more space around operators, but the rule for continuing a
group between `(` and `)` or `[` and `]` currently depends on
distinguishing operators from non-operators.

# Prior art
[prior-art]: #prior-art

Spaling notation takes a lot of inspriation from S-expression,
alternative S-expression notations, and especially
[Lexprs](https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/text/0004-lexpr.md).
The idea that, even in an S-expression-like setting, some parsing can
be deferred a later parser has many precedents, including Clojure's choice
of where to put parentheses and notations that use something like `$`
to escape to infix mode.

# Unresolved questions
[unresolved-questions]: #unresolved-questions


# Future possibilities
[future-possibilities]: #future-possibilities

Like other notation designs, this one leaves open exactly the way that
the notation would be used to express a new programming language. The
examples are meant to be suggestive and have influenced many of the
notational choices, though.
