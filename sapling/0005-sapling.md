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
unless Lisp/Scheme/Racket code has no line comments, it is
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

Here are all of the special tokens that are used for grouping:

```
: | &   ( ) [ ] { }   ; ,   \
```

Lines breaks, and particularly a blank line, are also meaningful for
grouping.

Here are some example saplings in standard indentation.

```
define pi = 3.14

define fib(n):
  log_error("fib called")
  cond | n = 0: 0
       | n = 1: 1
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
& property
  prop_equal_and_hash
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
    if m = 0
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
  for & (x = in_list(l),
         x2 = in_list(l2)):
    print(x)
    print_string(" ")
    print(x2)
    newline()

define show_cross(l, l2):
  for & x = in_list(l)
      & x2 = in_list(l2):
    print(x)
    print_string(" ")
    print(x2)
    newline()
```

Identifiers are Java-style with alphanumerics and underscores.
Operators are sequences of symbolic characters in the sense of
`char-symbolic?`, roughly. Numbers are written in some reasonable way
distinct from identifiers. No spaces are needed between operators and
non-operators, so `1+2` and `1 + 2` mean the same thing.

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

 - A `:` starts a nested sequence of groups, much in the same way that
   _opener_-_closer_ pairs create nested groups, but there's no
   explicit closer to go with `:`.

   ```
   define sqr(x):
      x*x
   ```

 - A blank line terminates all active groups up to an enclosing
   _opener_-_closer_ pair, so a blank line is one way to end a `:`
   group. A `,` or `;` has the same effect as a blank line, so `,` may
   be a common way to end a group within `(` and `)`, while `;` may be
   a more natural way to end a group within `{` and `}`. Enforcing a
   the use of `,` with `(` and `)` is up to a language that is written
   in sapling notation.

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

 - A `|` at the start of a line continues a group from the preceding
   line. It also starts nested groups in the same way as `:`. However,
   if a `|` anywhere has a preceding `|` within the same
   _opener_-_closer_ pair, then it also closes active groups up to
   that `|`. A blank line, `,`, or `;` meanwhile closes all active
   groups without stopping at a `|`. Overall, a blank line, `,`, or
   `;` is effectively stronger as a terminator than a `|`, but `|` is
   still strong enough to terminate a `:` if there's a `|` before the
   `:`.

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

 - A `&` is similar to a `|`, but `&` closes groups only up to a `&`,
   `|`, or `:`, whichever is first. In other words, `&` is weaker as a
   closer than `|`, and it nests inside `:` instead of closing a `:`.
   Another view is that `&` has a higher “precedence” than `|` and `:`
   for joining nested groups (while `:` has a higher “precedence” than
   `|`).

   ```
   define
   | make_list_of_adders(lo, hi):
      for list & (i = in_range(lo, hi)):
        lambda (n): i + n
   | make_list_of_adders(hi):
       make_list_of_adders(0, hi):
   ```

   The relative precedence of `:`, `&`, and `|` is meant to be
   convenient, but it won’t avoid the need for parentheses in all
   cases.

   ```
   define assoc(key, lst):
     match lst
     | empty: #false
     | cons(cons(a_key, a_val), rest):
         (cond
          | is_equal(a_key, key): a_val
          | else: assoc(key, rst))
   
   ```

 - As a last resort, a `\` at the end of the line (before
   whitespace/comments) continues the group into the next line.

   ```
     This group became \
       way too long \
       to put on a single line
   ```

 - In the special case of a group immediately within `(` and `)` or
   `[` and `]`, any operator at the start of a line continues the
   previous line. So, while `X + Y` cannot be split across
   lines—except by resorting to a `\`— the form `(X + Y)` can be split
   across lines by adding a newline before `+`, which is handy if `X`
   and `Y` stand for large expressions.

   ```
    lambda(first_arg, second_arg):
       sqrt(first_arg * first_arg
            + second_arg * second_arg)
   ```

 - The parsed form of `(` and `)` or `[` and `]` or `{` and `}`
   records the use of parentheses, square brackets, or curly braces,
   respectively. Similarly, the parsed form of `,` or `;` records the
   comma or semicolon as part of the enclosing group. A `\` silently
   continues a group on the next line, while `:` is recorded as part
   of the group that it continues.

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
  (#%grp (#%grp fib (#%paren (#%grp 0)) : (#%grp (#%grp 0))))
  \|
  (#%grp (#%grp fib (#%paren (#%grp 1)) : (#%grp (#%grp 1))))
  \|
  (#%grp
   (#%grp
    fib
    (#%paren (#%grp n))
    :
    (#%grp
     (#%grp fib (#%paren (#%grp n - 1)) + fib (#%paren (#%grp n - 2))))))))

define show_combos(l, l2):
  for & x = in_list(l)
      & x2 = in_list(l2):
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
    &
    (#%grp (#%grp x = in_list (#%paren (#%grp l))))
    &
    (#%grp (#%grp x2 = in_list (#%paren (#%grp l2))))
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

 * `:` at the end of a line indents nested groups to one step larger
   than the current group's indentation.

 * `:` at the start or middle of a line indents nested groups to one
   space after the `:`.

 * `|` indents nested groups to one space after the `|`.

 * `&` indents nested groups to one space after the `&`.

 * _opener_ at the start or middle of a line indents nested groups to
   line up after the _opener_.

 * _opener_ at the end of a line indents nested groups like `:` at the
   end of a line, unless it is immediately preceded by a `:`. If the
   corresponding _closer_ appears at the start of a line, it is
   indented line the group that is extended by _opener_.

 * `:` followed by _opener_ at the end of a line indents like _opener_
   or `:` at the end of a line. (The `:` and _opener_ can be separated
   by whitespace and comments.) If the corresponding _closer_ appears
   at the start of a line, it is indented line the group that is
   extended by `:`.

 * `\` at the end of a line indents the continuation one step deeper
   than the current group's indentation, unless the previous line was
   already a `\` continuation for the same group, in which case the
   next line uses the current line's indentation.

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
the way that an `:` has a higher precdence than a `&` or `|` and `&`
has a higher precedence than `|`.

The lexeme-level syntax here would require some sort of bridge to
Racket names that don’t fit that syntax. For example, something like
`#{....}` could be the syntax for an identifier that has any character
between the curly braces (with some suitable generalization to
accomodate `{` and `}` in identifier names), so `#{exact-integer?}`
would be an identifier with `-` and `?` as part of the identifier.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The lexeme-level syntax is chosen to be familiar to programmers
generally. The sequence `1+2` is one plus two, not a strangely spelled
identifier. Extra parenthesese are always allowed (pending the
sampling consumer's cooperation) and can be freely used to
disambiguate grouping. Tokens like `(`, `,`, `{` and `;` are used in
familar ways. Saplings provide enough grouping structure that
code-navigaton and -transformation should be useful and straighforward
in an editor. You can select a range of code to reindent if editing
has made it unreadable.

A full sapling-notation design should incopoprate `at-exp` notation,
too (where `@` escapes returrn to sapling notation instead of
S-expressions).

There may be room to tweak the grouping rules to avoid unnecessary
`#%grp`s.

Sapling notation could be adapted to support Lisp-style identifiers by
requiring more space around operators, but the rule for continuing a
group between `(` and `)` or `[` and `]` currently depends on
distinguishing operators from non-operators.

# Prior art
[prior-art]: #prior-art

Sapling notation takes a lot of inspriation from S-expressions,
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
