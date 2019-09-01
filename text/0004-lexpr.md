- Feature Name: Line-expressions as fundamental syntax
- Start Date: 2019-08-30
- RFC PR: [racket/racket2-rfcs#XXX](https://github.com/racket/racket2-rfcs/pull/XXX)

# Summary
[summary]: #summary

Line-expressions (L-expressions or Lexprs) are like S-expressions, but
with more syntactic categories and without as much required
notation. Like S-expressions, they mostly lack a semantic
interpretation. Lexprs are reminiscent of many aspects of existing
languages, but have a unique flavor.

# Motivation
[motivation]: #motivation

The uniformity of S-expressions limits the amount of information at
the notational level of reading Racket programs. A small amount of
extra notation can go a long way with a small number of mores on its
use. For example, in Racket brackets are used in S-expressions when no
function or macro application is implied (like in the cases of a
`cond`); reading Racket programs without this notational affordance is
more difficult. Similarly, blocks are typically distinguished in
Racket by indenting their bodies after the first line and it is very
difficult to read Racket programs that don't follow this convention.

Many Racket programs use mathematics but cannot be written in the
traditional infix notation that is parsed using the PMDASFLTR
(parentheses, multiplication, division, addition, subtraction, from
left to right) algorithm common across the world.

It is awkward to embed arbitrary fragments of code not in S-expression
format, such as when quoting a program in another language. The only
effective option is to embed a string. The Racket @-reader is helpful
at this, but it is not uniformly available and the standard structure
of Racket's S-expression based languages do not allow macro-specific
reading of such syntaxes.

Line-expressions are an alternative semantics-free tree structure with
more built-in categories. We expect that this notation will enable
language builders in Racket, such as the community for Racket2, to
create many useful mores for writing and reading Racket2 code. Lexprs
facilitate mathematical infix notation and the embedding of non-Lexpr
syntax.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The reference section gives many more examples of the exact structure
of line-expressions. In this section, we give a few broad examples and
demonstrate the structure of Lexprs.

Lexprs are divided into leaders, units, sequences and groups, lines,
and text. Leaders are distinguished by their first characters, like
identifier, numbers, and embedded versions of the other
categories. Units are leaders followed by another character, such as
`(` which opens a group or `.` which precedes another unit. Sequences
are a series of groups separated by commas. Groups are a series of
units separated by spaces and parsed with infix notation. Lines are a
series of units separated by spaces that ends in a follower. Followers
are tokens like a newline, which end a line, or `&`, which extend a
line past a newline, or `:`, which embeds a series of lines indented
one level, or `|`, which embeds a series of lines aligned with the
bar, and so on. Text is delimited by `{` and `}` and may escape with
`@` and is always parsed into lists of characters split across `\n`.

In general, Lexprs are very strict on their formatting: additional
spaces are never allowed and newlines are meaningful. In Lexprs, a
level of indentation is always exactly two space characters: `  `.

Here is an extended example that demonstrates many of the formats in
Lexprs:

```lexpr
fun ksum(k, l) :
  match l \
    | empty :
        0
    | cons(a, d) :
        (a + k * ksum(k, d))
        
fun timed_thunk(thunk) :
  let before = now()
  let answer = thunk()
  let after = now()
  println {It took @(after - before) seconds}
  answer
  
mac timed \
  | [_ e] :
      'timed_thunk([λ() : e])
```

It is parsed into the following AST:

```sexpr
((#%line
  fun (#%fun-app ksum k l)
  (#%indent
   (#%line match l
           (#%bar (#%line empty
                          (#%indent (#%line 0)))
                  (#%line (#%fun-app cons a d)
                          (#%indent (#%line (+ a (* k (#%fun-app ksum k d))))))))))
 (#%line
  fun (#%fun-app timed_thunk thunk)
  (#%indent
   (#%line let before = (#%fun-app now))
   (#%line let answer = (#%fun-app thunk))
   (#%line let after = (#%fun-app now))
   (#%line println (#%text ("It took " (#%text-esc (- after before)) " seconds")))
   (#%line answer)))
 (#%line
  mac timed
  (#%bar
   (#%line
    (#%line _ e)
    (#%indent
     (#%line
      (#%quote
       (#%fun-app timed_thunk
                  (#%line (#%fun-app λ)
                          (#%indent (#%line e)))))))))))
```

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

This section presents a large number of examples of Lexprs, each with
its parsed version. The parsed versions are checked by the reference
implementation, which is attached to this pull request in
[p.rkt](0004-lexpr/p.rkt).

Lexprs are defined relative to UTF-8, so the word "character" in this
document does not mean one byte, but rather one UTF-8 encoded
character.

The body of a module is a sequence of lines. In general, sequences are
not annotated in Lexpr ASTs, but lines are annotated with `#%line`.

## Numbers

Numbers are leaders and can include explicit signs or a single `.`:

```lexpr
1
```
`=>`
```sexpr
((#%line 1))
```

```lexpr
+1
```
`=>`
```sexpr
((#%line 1))
```

```lexpr
-1
```
`=>`
```sexpr
((#%line -1))
```

```lexpr
3.14
```
`=>`
```sexpr
((#%line 3.14))
```

Unlike Racket, there are no special notations for complex numbers, different
exactitudes of numbers, or different bases. We expect these notations
to use a library of text applications, like `0b{0110}`.

## Symbols

Symbols are the most common leaders are any sequence of characters
that is not a number and does not include a character in `\n
.,'()[]{}⟨⟩`. As a special case, a symbol may begin with `.`.

```lexpr
x
```
`=>`
```sexpr
((#%line x))
```

```lexpr
0b
```
`=>`
```sexpr
((#%line 0b))
```

```lexpr
.
```
`=>`
```sexpr
((#%line |.|))
```

```lexpr
<=
```
`=>`
```sexpr
((#%line <=))
```


## Characters

Literal characters are written with `#\` followed by a character.

```lexpr
#\

```
`=>`
```sexpr
((#%line #\newline))
```

```lexpr
#\n
```
`=>`
```sexpr
((#%line #\n))
```

Unlike Racket, there are no "named" characters, `#\newline`. We expect
these to be provided by a library of definitions which simply name
characters. They would be referred to in text with something like
`@newline` or `{The character for a smile is @(smiley-face)!}`.

## Grouping

A leader that starts with `(` is a group: a sequence of units
separated by spaces parsed with infix notation. The group ends with a
`)`. The presence of a group is not visible in the resulting AST.

The infix order of operations is based on the basic mathematical
operations, PMDASFLTR (parentheses, multiplication, division,
addition, subtraction, from left to right), with a very small number
of extra operators. All operators associate the same way---to the
left.

The following table shows the order of operations from tightest to
loosest. If two operators appear in the same group, then they cannot
be combined together in the same group. This is to prevent confusing
combinations.

| `:` |
| `*` |
| `/` |
| `%` |
| `+` |
| `-` |
| operators not mentioned |
| `< <= == != >= >` |
| `&& ||` |
| `.` |
| `$` |
| `=` |
| `=>` |

An operator is any sequence of characters with no characters in the
Unicode categories `Ll`, `Lu`, `Lt`, `Lm`, and `Lo`.

This table is designed to allow variables to be annotated with `:`,
then combined with arithmetic operations (`*/%+-`), then compared
against other equations (`==`, etc), those boolean equations combined
with others (`&&` and `||`) and finally assigned (`=`).

In parallel, the table supports curried functional programming and
pointless function operations like `.` and `$`, because adjacency is
treated as function application

Here are some examples:

All operators are left-associative:

```lexpr
(x + 6 + y)
```
`=>`
```sexpr
((#%line (+ (+ x 6) y)))
```

Multiplication is tighter than addition:

```lexpr
(x + 6 * y)
```
`=>`
```sexpr
((#%line (+ x (* 6 y))))
```

An embedded group can change the order of operations:

```lexpr
((x + 6) * y)
```
`=>`
```sexpr
((#%line (* (+ x 6) y)))
```

Groups are not visible in the output, so they can be added
unnecessarily:

```lexpr
((((x) + 6)) * (y))
```
`=>`
```sexpr
((#%line (* (+ x 6) y)))
```

`:` is an operator, as well as a line follower.

```lexpr
(x : int)
```
`=>`
```sexpr
((#%line (: x int)))
```

Here is a prototypical mathematical group:

```lexpr
(ans = 3 * x + y / 4 <= z % 3 && 2 != 5)
```
`=>`
```sexpr
((#%line (= ans (&& (<= (+ (* 3 x) (/ y 4)) (% z 3))
                    (!= 2 5)))))
```

Here are some prototypical functional programming groups:

```lexpr
(add1 . mult2 $ 5)
```
`=>`
```sexpr
((#%line ($ (|.| add1 mult2) 5)))
```

and

```lexpr
(sub1 . length . map add1 $ iota 4)
```
`=>`
```sexpr
((#%line ($ (|.| (|.| sub1 length)
                 (#%fun-app map add1))
            (#%fun-app iota 4))))
```

Operators are the same precedence level cannot be mixed without
parentheses:

```lexpr
(1 + 2 <= 3 && false || true)
```
`=>`
```sexpr
(error "Operators with same precedence cannot be used in the same group: || and &&")
```

```lexpr
((1 + 2 <= 3 && false) || true)
```
`=>`
```sexpr
((#%line (\|\| (&& (<= (+ 1 2) 3) false) true)))
```

```lexpr
(1 < 2 == 3)
```
`=>`
```sexpr
(error "Operators with same precedence cannot be used in the same group: == and <")
```

Operators not otherwise mentioned are between arithmetic and
comparison operators:

```lexpr
(0 <= 1 ⊕ 2 + 3)
```
`=>`
```sexpr
((#%line (<= 0 (⊕ 1 (+ 2 3)))))
```

And they are all at the same precedence level, so they cannot be
mixed:

```lexpr
(1 ⊕ 2 ⊗ 3)
```
`=>`
```sexpr
(error "Operators with same precedence cannot be used in the same group: ⊗ and ⊕")
```

This particular example shows how this is unfortunate, but Lexprs try
to strike a balance between enabling common patterns and avoiding
program-specific parsing. XXX Alternative

This example shows how operators might be used in the syntax of a
function definition:

```lexpr
λ(x, y, def = 5, kw_ext1 => kw_int1, kw_ext2 => kw_int1 = 6, ... rest) :
  {x, y, and kw_ext1 are mandatory}
  {x, y, def, and rest are passed by position}
  {kw_ext1 and kw_ext2 are passed by keyword}
```
`=>`
```sexpr
((#%line
  (#%fun-app
   λ
   x
   y
   (= def 5)
   (=> kw_ext1 kw_int1)
   (=> kw_ext2 (= kw_int1 6))
   (#%fun-app (#%dot |.| |.|) rest))
  (#%indent
   (#%line (#%text ("x, y, and kw_ext1 are mandatory")))
   (#%line (#%text ("x, y, def, and rest are passed by position")))
   (#%line (#%text ("kw_ext1 and kw_ext2 are passed by keyword"))))))
```

## Dots

A leader followed by a `.` and another unit is a dot unit:

```lexpr
x.y
```
`=>`
```sexpr
((#%line (#%dot x y)))
```

Since the left is a leader, this means that dots are left-associative:

```lexpr
x.y.z
```
`=>`
```sexpr
((#%line (#%dot x (#%dot y z))))
```

Since `.` is a valid identifier, it is a valid leader, which means
`...` is a unit:

```lexpr
...
```
`=>`
```sexpr
((#%line (#%dot |.| |.|)))
```

## Applications

There are three major kinds of applications in Lexpr notation. Each is
a leader followed by an open symbol, a sequence, then a close
symbol. The valid open/close pairs are `()`, `[]`, and `⟨⟩`. The first
is called function application, the second is member access, and the
third is parameter application. These names are suggestive and
particular Lexpr-based languages can give them any semantics.

The basic form of function application:

```lexpr
f(x)
```
`=>`
```sexpr
((#%line (#%fun-app f x)))
```

Since the body is a sequence, there can be multiple groups separated
by commas:

```lexpr
f(x, y)
```
`=>`
```sexpr
((#%line (#%fun-app f x y)))
```

Since a sequence is made of groups, prefix notation is enabled without
the use of `()`s:

```lexpr
f(x + 2, y)
```
`=>`
```sexpr
((#%line (#%fun-app f (+ x 2) y)))
```

If `[]` are used instead of `()`, then it is apparent in the AST:

```lexpr
f[x, y]
```
`=>`
```sexpr
((#%line (#%member f x y)))
```

Similarly for `⟨⟩`:

```lexpr
f⟨x, y⟩
```
`=>`
```sexpr
((#%line (#%param f x y)))
```

An application is a unit, and another unit may follow, so applications
may nest arbitrarily. They are left-associative (as if everything in
Lexprs!)

```lexpr
f⟨A, B⟩(x, y)[1, 2]
```
`=>`
```sexpr
((#%line (#%member (#%fun-app (#%param f A B) x y) 1 2)))
```

## Quotation

Lexprs have a quote form, `'`, that wraps the next unit in a special
AST `#%quote` node. If a leader is `,`, then a unit follows which is
wrapped in a special `#%unquote` node.

All normal parsing takes place inside of quotations, including prefix notation:

```lexpr
(x + '(y * 6) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (* y 6))) z)))
```

As well as dots:

```lexpr
x + 'x.y + z
```
`=>`
```sexpr
((#%line x + (#%quote (#%dot x y)) + z))
```

And function application:

```lexpr
(x + 'f(x) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f x))) z)))
```

An unquote is always in the leader position, and a comma in a sequence
is always in the follower position, so there is never ambiguity about
where a comma is an unquote or the next element in a sequence:

This `,` is an unquote:

```lexpr
(x + 'f(x + ,y) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote y))))) z)))
```

This example has both kinds of commas:

```lexpr
(x + 'f(x + ,y, a) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote y)) a))) z)))
```

Unquote consumes an entire unit, which may include more commas, etc,
this is uncontroversial:

```lexpr
(x + 'f(x + ,g(7, y)) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote (#%fun-app g 7 y)))))) z)))
```

## Text Quotation

In Lexprs, matched `{}` delimit a text quotation. The quotation may
run any number of lines and may include `@`s which escape a single
unit back into Lexpr mode. Text quotations do not provide any
interpretation on any character sequences other than `@`. The entire
quotation is present in the AST as `#%text` with one list of per line
(i.e. per `\n` character). Escapes are tagged with `#%text-esc`.

Here is a simple example:

```lexpr
{Hello World!}
```
`=>`
```sexpr
((#%line (#%text ("Hello World!"))))
```

And one with an escape:

```lexpr
{Hello @(1 + 2)!}
```
`=>`
```sexpr
((#%line (#%text ("Hello " (#%text-esc (+ 1 2)) "!"))))
```

Here's how you might choose to write an `@` sign:

```lexpr
{Hello @(#\@)!}
```
`=>`
```sexpr
((#%line (#%text ("Hello " (#%text-esc #\@) "!"))))
```

Or a newline:

```lexpr
{Hello @(newline)!}
```
`=>`
```sexpr
((#%line (#%text ("Hello " (#%text-esc newline) "!"))))
```

However, you can just include newlines:

```lexpr
{Hello

 World!}
```
`=>`
```sexpr
((#%line (#%text ("Hello") () (" World!"))))
```

Text quotations can be empty:

```lexpr
{}
```
`=>`
```sexpr
((#%line (#%text ())))
```

And remember, only a single unit is included, not a group:

```lexpr
{@1 + 2!}
```
`=>`
```sexpr
((#%line (#%text ((#%text-esc 1) " + 2!"))))
```

There's nothing wrong with embedded braces in the quotation:

```lexpr
{This is a { embedded brace! } }
```
`=>`
```sexpr
((#%line (#%text ("This is a " "{" " embedded brace! " "}" " "))))
```

## Text applications

When a `{` is a follower, then it is a text application unit, which is
notated with a special AST:

```lexpr
let x = item{Some text}
```
`=>`
```sexpr
((#%line let x = (#%text-app item ("Some text"))))
```

This is particular convenient inside text quotations:

```lexpr
{Here is some @bold{text} with some @color[1.0, 0.0, 0.0]{RED} letters.}
```
```sexpr
((#%line (#%text ("Here is some " (#%text-esc (#%text-app bold
("text"))) " with some " (#%text-esc (#%text-app (#%member color 1.0
0.0 0.0) ("RED"))) " letters."))))
```

## Lines

We now comes to lines. At a first glance, lines are just sequences of
units separated by spaces.

```lexpr
x y
```
`=>`
```sexpr
((#%line x y))
```

Since they are units, there is no prefix parsing:

```lexpr
x + y
```
`=>`
```sexpr
((#%line x + y))
```

Unless enabled by `()`s:

```lexpr
x + (y + 3)
```
`=>`
```sexpr
((#%line x + (+ y 3)))
```

However, everything interesting about lines is determined by their
follower.

# Line Follower: Newline

If a line follower is a newline (i.e. `\n`), then the line is
terminated and a new line is parsed:

```lexpr
foo bar
zig zag
```
`=>`
```sexpr
((#%line foo bar) (#%line zig zag))
```

A blank line, at the top-level, is ignored:

```lexpr
foo bar

zig zag
```
`=>`
```sexpr
((#%line foo bar) (#%line zig zag))
```

## Line follower: Slash

A `\` line follower continues the line AST passed a newline, with one
required level of indentation.

```lexpr
foo \
  bar
zig zag
```
`=>`
```sexpr
((#%line foo bar) (#%line zig zag))
```

The line is not just a single unit, but could be many:

```lexpr
foo \
  bar baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

The exact way that a line was written in the source code is not
apparent in the AST:

```lexpr
foo bar \
  baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

Of course, any line follower may be used multiple times:

```lexpr
foo \
  bar \
    baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

## Line follower: Colon

A `:` line follower embeds a series of lines inside the given line at
one required level of indentation and continues the line after the
level of indentation terminates.

Here's a typical use case:

```lexpr
if (x < y) :
  f(x)
else :
  g(y)
```
`=>`
```sexpr
((#%line if (< x y) 
   (#%indent (#%line (#%fun-app f x)))
  else 
   (#%indent (#%line (#%fun-app g y)))))
```

The indented region may be multiple lines: 

```lexpr
zig :
  zag
  zog
```
`=>`
```sexpr
((#%line zig (#%indent (#%line zag) (#%line zog))))
```

But blank lines are not allowed. XXX

```lexpr
zig :
  zag
  
  zog
```
`=>`
```sexpr
(error "unexpected")
```

As a special case, the first line in the series may begin immediately
after the `:` as in this example:

```lexpr
zig : zag
  zog
```
`=>`
```sexpr
((#%line zig (#%indent (#%line zag) (#%line zog))))
```

Of course, multiple line followers may be used in tandem:

```lexpr
a :
  b \
    c
  d
f
g
```
`=>`
```sexpr
((#%line a (#%indent (#%line b c) (#%line d)) f) (#%line g))
```

## Line follower: At

XXX Boundary

XXX Line follower: @

```lexpr
foo bar @
  This is just some text!
  It can have many lines
  And @(4 - 3) quote!
baz
```
`=>`
```sexpr
((#%line 
  foo
  bar
  (#%text ("This is just some text!")
          ("It can have many lines")
          ("And " (#%text-esc (- 4 3)) " quote!"))
  baz))
```

XXX Line follower: &

```lexpr
foo \
  bar &
  baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

```lexpr
foo \
  bar &
  baz &
  zab
zig zag
```
`=>`
```sexpr
((#%line foo bar baz zab) (#%line zig zag))
```

XXX Line follower: |

```lexpr
let | x = 1
    | y = 2
in :
  x + y
```
`=>`
```sexpr
((#%line let (#%bar (#%line x = 1) (#%line y = 2)) in (#%indent (#%line x + y))))
```

```lexpr
a :
  b \
    | e

g

i
```
`=>`
```sexpr
((#%line a (#%indent (#%line b (#%bar (#%line e)))))
 (#%line g)
 (#%line i))
```

XXX Embedded lines

```lexpr
foo bar [zig zag]
```
`=>`
```sexpr
((#%line foo bar (#%line zig zag)))
```

```lexpr
foo bar [zig [baz] zag]
```
`=>`
```sexpr
((#%line foo bar (#%line zig (#%line baz) zag)))
```

```lexpr
foo bar [zig \
           zag] baz
```
`=>`
```sexpr
((#%line foo bar (#%line zig zag) baz))
```

```lexpr
foo bar [zig :
           zag
           zog] baz
```
`=>`
```sexpr
((#%line foo bar (#%line zig (#%indent (#%line zag) (#%line zog))) baz))
```

XXX MORE goal: only one way to format
--- not really, because of \ and precedence

XXX MORE sacred cow: comments

Don't have comments and insist on literal programming
for prose, plus logging, tests ("Show, don't tell"), good names,
specification, etc, because the compiler doesn't execute or analyze
comments.

Problem 1.a: What about commenting out code?
Solution 1.a.1: (when false ...) is easy, but doesn't work for
macros.
Solution 1.a.2: `git diff` and the kill ring has the knowledge you
want

XXX ; as a line follower or leader?

XXX sacred cow: no extra whitespace, except for newlines

Problem: Lining up definitions or other things, like:
  let foo = 1
        a = 2
Solution 1: Allow multiple |s?
let | foo | 1
    |   a | 2
Problem 1.1: Formatting is annoying to maintain and must be fixed
up.
Modification 1.2: Treat = (and =>?) as another kind of balancer,
like | because it is common.

XXX lots of overlap with `#lang something`: https://github.com/tonyg/racket-something

XXX <> is ugly special casing... maybe remove #%param-app?
