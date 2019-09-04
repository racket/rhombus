- Feature Name: Sapling notation
- Start Date: 2019-09-04
- RFC PR: [racket/racket2-rfcs#115](https://github.com/racket/racket2-rfcs/pull/1145)

# Summary
[summary]: #summary

Sapling notation is similar to S-expression notation, but instead of
generating fully formed trees, it is intended to partially group input
for further enforestation by another parser (e.g., as in Honu). The
notation is line-sensitive but indentation-insensitive. The identation
of a sapling can be normalized using only lexemes without further
parsing information. The parsed form of a sapling imposes grouping to
ensure that further parsing is consistent with the Salping's normal
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
to ensure that line breaks and indentation in the source <are not
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

define fib(n) =
  log_error("fib called")
  cond | (n = 0) => 0
       | (n = 1) => 1
       | else => fib(n-1) + fib(n-2)

define
| fib(0) = 0
| fib(1) = 1
| fib(n) = fib(n-1) + fib(n-2)

define fib =
  lambda (n) =>
    cond | (n = 0) => 0
         | (n = 1) => 1
         | else => fib(n-1) + fib(n-2)

define fib(n) =
  match n
  | 0 => 0
  | 1 => 1
  | n => fib(n-1) + fib(n-2)

define fib(n) =
  match n
  | 0 =>
      0
  | 1 =>
      1
  | n =>
      fib(n-1) + fib(n-2)

define make_adder(n) =
  lambda (m) =>
    printf("adding to ~a\n", m)

define fourth(n : integer) =
  define m = n*n
  define v = m*m
  printf("~a^4 = ~a\n", n, v)
  v

struct posn(x, y):
  _property prop_equal_and_hash \
    (let | hc => lambda (a : posn, hc) =>
                   hc(a.x) + hc(a.y)
     & [lambda (a : posn, b : posn, eql) =>
          (eql(a.x, b.x) \
             && eql(a.y = b.y)),
        hc,
        hc])

define go() = {
  define helper(n) =
    list(n, n)

  define more(m) =
    if (m == 0)
    | "done"
    | more(m - 1)

  helper(more(9))
}

define curried =
  lambda (x) =>
    lambda (y) =>
      lambda (z) =>
        list(x, y, z)

dictionary = {"foo" : 17,
              "bar" : "string",
              "baz" : #true }

begin:
  printf("Creating a dictionary\n")
  dictionary = {
    "foo" : 17,
    "bar" : "string",
    "baz" : #true
  }

let | x = 1
    | y = 2
& printf("About to add")
  x+y
```

Identifiers are Java-style with alphanumerics and underscores.
Operators are sequences of symbolic characters in the sense of
`char-symbolic?`, roughly. Some operators, including `|` and `=>`, are
treated specially for indentation and grouping, but that doesn't
preclude their use like other operators. Numbers are written in some
reasonable way distinct from identifiers. No spaces are needed between
operators and non-operators, so `1+2` and `1 + 2` mean the same thing.
Function calls, recognized as having no white space between an expression
and open parenthesis, are distinct from other forms at the reader
level.

Special syntactic tokens:

 - An _arrow_ is an operator that includes `<` or `>`.

 - An _equal_ is an operator that ends in `=` or `:`
   and has no `<` or `>`.

 - An _opener_ is a `(`, `[`, or `{`, or maybe an Unicode opener.

 - A _closer_ is a `)`, `]`, or `}`, or maybe any Unicode closer.

 - A _conj_ is `|` or `&`.

Grouping overview:

 - Pairs of _opener_ and _closer_ contain a sequence of groups. No
   group within the _opener_-_closer_ pair extends outisde the pair.

 - A blank line terminates all currently enclosing groups up to the
   enclosing _opener_-_closer_ pair. A `,` or `;` has the
   same effect.

 - A _conj_ ends all nested groups up to the previous _conj_, if any,
   within an enclosing _opener_-_closer_ pair. It then continues any
   current group that remains open. So, pairs of _conj_ bracket groups
   in a similar way to _opener_-_closer_ pairs, but a blank line, `,`,
   or `;` is stronger.

 - A line-ending _equal_ or line-ending/middle/starting _arrow_
   continues the current group into the new line, but starts a nested
   sequence of subgroups for the parts after the _equal_ or _arrow_.
   Nested subgruped are ended by a _closer_, blank line, `,`,
   `;`, or _conj_ — possibly ending multiple groups at once.

 - Unless continued with a _equal_, _arrow_, or _conj_, each line
   starts a new group.

Here are some saplings each followed by the corresponding parsed forms
as represented by an S-expression:

```
define pi = 3.14

(#%all (#%grp define pi = 3.14))

define
| fib(0) = 0
| fib(1) = 1
| fib(n) = fib(n-1) + fib(n-2)

(#%all
 (#%grp
  define
  \|
  (#%grp (#%call fib (#%grp 0)) = 0)
  \|
  (#%grp (#%call fib (#%grp 1)) = 1)
  \|
  (#%grp
   (#%call fib (#%grp n))
   =
   (#%call fib (#%grp n - 1))
   +
   (#%call fib (#%grp n - 2)))))

begin:
  fprintf(current_output_port(), "hello\n")
  exit(1)

(#%all
 (#%grp
  begin
  :
  (#%grp
   (#%grp
    (#%call
     fprintf
     (#%grp (#%call current_output_port))
     |,|
     (#%grp "\"hello\\n\"")))
   (#%grp (#%call exit (#%grp 1))))))
```

In a Racket implementation of a language based on saplings, the intent
is that `#%grp` acts like `begin` on expression and definition, where you
can always wrap more of them.

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

See [sapling.rkt](sapling.rkt). Note that if you run with no
arguments, the that program will read from stdin and not write
anything. Use flags like `--reindent` or `--group` to get output.
Supply one or more files are arguments to read from those files.

See [demo.sap](demo.sap), [interp.sap](interp.sap), and
[weird.sap](weird.sap) for more examples.

# Drawbacks
[drawbacks]: #drawbacks

Sapling notation is not a good choice where precise and complete
grouping is needed, both because its grouping is coarse-grained and
the grpuing rules generate lots of extra `#%grp` layers.

Saplings do not resolve the question of how infix expressions parse.
There is no precedence at the sapling level, for example, other than
the way that an _arrow_ has a higher precdence than a _conj_.

Not being able to split `(1 + 2)` across lines without a `\` seems
awkward.

The lexeme-level syntax here would require some sort of bridge to
Racket names that don't fit that syntax.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

The lexeme-level syntax is chosen to be familiar and to distniguish
function calls from other kinds of syntactic forms. The sequence `1+2`
is one plus two, not a strangely spelled identifier. Extra
parenthesese are always allowed (pending the sampling consumer's
cooperation) and can be freely used to disambiguate grouping. Tokens
like `:`, `{`, and `=` are used in familar ways. Saplings provide
enough grouping structure that code-navigaton and -transformation
should be useful and straighforward in an editor. You can select a
range of code to reindent if editing has made it unreadable.

There is a lot of room to change the definition of _arrow_, _equal_,
etc., to adjust the indentation rules, and possibly to tweak the
grouping rules to avoid unecessary `#%grp`s.

# Prior art
[prior-art]: #prior-art

Spaling notation takes a lot of inspriation from S-expression,
alternative S-expression notations, and especially
[Lexprs](https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/text/0004-lexpr.md).
The idea that, even in an S-expression-like setting, some parsing can
be deferred a later arser has many precents, including Clojure's choice
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
