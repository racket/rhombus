- Feature Name: Line-expressions as fundamental syntax
- Start Date: 2019-08-30
- RFC PR: [racket/racket2-rfcs#114](https://github.com/racket/racket2-rfcs/pull/114)

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
`@` and is always parsed into lists of strings split across `\n`.

In general, Lexprs are very strict on their formatting: additional
spaces are never allowed and newlines are meaningful. In Lexprs, a
level of indentation is always exactly two space characters: `  `.

Here is an extended example that demonstrates many of the formats in
Lexprs:

```lexpr
fun ksum(k, l) :
  match l \
    | empty :
        ; This is my favorite number!
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
left. All operators are binary.

The following table shows the order of operations from tightest to
loosest. If two operators appear in the same group, then they cannot
be combined together in the same group, unless noted. This is to prevent confusing
combinations.

| Operators | Note |
| --------- | ---- |
| `:` | |
| `*` `*` `/` | may be combined |
| `+` `-` | may be combined |
| operators not mentioned |
| `< <= == != >= >` | |
| `&& \|\|` | |
| `.` | |
| `$` | |
| `=` | |
| `=>` | |

An operator is any sequence of characters with no characters in the
Unicode categories `Ll`, `Lu`, `Lt`, `Lm`, and `Lo`.

This table is designed to allow variables to be annotated with `:`,
then combined with arithmetic operations (`*/%+-`), then compared
against other equations (`==`, etc), those boolean equations combined
with others (`&&` and `||`) and finally assigned (`=`).

In parallel, the table supports curried functional programming and
pointless function operations like `.` and `$`, because adjacency is
treated as function application, even if operators appear alone.

Here are some examples:

All operators are left-associative:

```lexpr
(x + 6 + y)
```
`=>`
```sexpr
((#%line (+ (+ x 6) y)))
```

Typical arithmetic formulas have expected meaning:

```lexpr
(1 + 2 - 3 + 4)
```
`=>`
```sexpr
((#%line (+ (- (+ 1 2) 3) 4)))
```

```lexpr
(1 + 2 * 3 + 4)
```
`=>`
```sexpr
((#%line (+ (+ 1 (* 2 3)) 4)))
```

```lexpr
(1 * 2 + 3 * 4)
```
`=>`
```sexpr
((#%line (+ (* 1 2) (* 3 4))))
```

```lexpr
(1 * 2 / 3 * 4)
```
`=>`
```sexpr
((#%line (* (/ (* 1 2) 3) 4)))
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

Adjacency means function unary application, even for operators:

```lexpr
(1 + 2 + (- 3) + 4)
```
`=>`
```sexpr
((#%line (+ (+ (+ 1 2) (#%fun-app - 3)) 4)))
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

Operators at the same precedence level cannot be mixed without
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
program-specific parsing.

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

Since the left is a leader and the right is a unit, this means that
dots are right-associative:

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

An application is treated like a leader, so anything that may follow a
leader is allowed, such as another application or a dot. In a sense,
this means that applications are right-associative.

```lexpr
f⟨A, B⟩(x, y)[1, 2].z
```
`=>`
```sexpr
((#%line (#%dot (#%member (#%fun-app (#%param f A B) x y) 1 2) z)))
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
whether a comma is an unquote or the next element in a sequence:

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

## Line Follower: Newline

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

## Line follower: And

An `&` line follower extends a line passed a newline without requiring
a new level of indentation.

```lexpr
foo &
bar &
baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

When combined with other line followers, the `&` requires the same
amount of indentation as the line it is continuing:

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

And it may be used multiple times:

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

But blank lines are not allowed.

```lexpr
zig :
  zag

  zog
```
`=>`
```sexpr
(error "unexpected")
```

Unless they include the correct indentation level (in which case, they
weren't actually empty):

```lexpr
zig :
  zag
  
  zog
```
`=>`
```sexpr
((#%line zig (#%indent (#%line zag) (#%line zog))))
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

This looks particularly tasty when used with `if`:

```lexpr
if (x < y) : f(x)
else : g(y)
```
`=>`
```sexpr
((#%line if (< x y) 
   (#%indent (#%line (#%fun-app f x)))
  else 
   (#%indent (#%line (#%fun-app g y)))))
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

Since a symbol may include line follower characters like `:`, it is
not possible to use them as a symbol inside a line, unless they are
wrapped in parens.

```lexpr
a : b
```
`=>`
```sexpr
((#%line a (#%indent (#%line b))))
```

vs

```lexpr
a (:) b
```
`=>`
```sexpr
((#%line a : b))
```


## Line follower: At

An `@` line follower begins an indented text quotation. The entire
indented region that follows is parsed as if it were between `{}`,
except that it must have leading indentation, which is not part of the
quotation.  Like `:`, the line continues after the indented region
terminates.

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

## Line follower: Bar

The `|` line follower begins a series of lines which must begin with
spaces and then a `|` at the same column as the original `|`. The
first line begins immediately after the `|`. Like `:`,  the line
continues after the `|` sequence terminates.

```lexpr
let | x = 1
    | y = 2
in :
  (x + y)
```
`=>`
```sexpr
((#%line let (#%bar (#%line x = 1) (#%line y = 2)) in (#%indent (#%line (+ x y)))))
```

As usual, all of the various line followers may be used at the same
time. If a line follower appears after a `|`, then the required
indentation begins at the first column of the line after the `|`.

```lexpr
a :
  b \
    | e
    | f \
        1
    | 2

g

i
```
`=>`
```sexpr
((#%line a (#%indent (#%line b (#%bar (#%line e) (#%line f 1) (#%line 2)))))
 (#%line g)
 (#%line i))
```

## Embedded lines

In leader position, `[]`s delimit an embedded line:

```lexpr
foo bar [zig zag]
```
`=>`
```sexpr
((#%line foo bar (#%line zig zag)))
```

They can, of course, be nested:

```lexpr
foo bar [zig [baz] zag]
```
`=>`
```sexpr
((#%line foo bar (#%line zig (#%line baz) zag)))
```

Or use line followers:

```lexpr
foo bar [zig \
           zag] baz
```
`=>`
```sexpr
((#%line foo bar (#%line zig zag) baz))
```

Of any kind:

```lexpr
foo bar [zig :
           zag
           zog] baz
```
`=>`
```sexpr
((#%line foo bar (#%line zig (#%indent (#%line zag) (#%line zog))) baz))
```

Or appear inside of quotes:

```lexpr
foo bar '[zig :
            zag
            zog] baz
```
`=>`
```sexpr
((#%line foo bar (#%quote (#%line zig (#%indent (#%line zag) (#%line zog)))) baz))
```

## Comments

Line expressions comments are written in two ways. First, a `#;`
leader consumes a unit, a space, and then another leader, returning the
second unit. Next, a `;` line follower is treated exactly like a new
line.

```lexpr
foo #;bar zog
```
`=>`
```sexpr
((#%line foo zog))
```

`#;` combines nicely with `{}` for long block comments.

```lexpr
foo #;{A long and beautiful text comment} zog
```
`=>`
```sexpr
((#%line foo zog))
```

Here's an example of `;` follower:

```lexpr
foo bar ; Here is a little note about programming style
zog zig ; I'm in favor of it!
```
`=>`
```sexpr
((#%line foo bar) (#%line zog zig))
```

And using it on its own, which acts like a blank line:

```lexpr
zig :
  zag
  ; This line is intentionally left blank.
  zog
```
`=>`
```sexpr
((#%line zig (#%indent (#%line zag) (#%line zog))))
```

# Drawbacks
[drawbacks]: #drawbacks

Line-expressions are strictly less powerful than a Honu-like syntax
where grouping is binding-sensitive and macros can control how much of
the token stream they read. This is trade-off that exchanges power for
an easier to remember set of rules.

Line-expressions rely on an operator precedence hierarchy, which many
people find distasteful. Line-expressions attempt to alleviate this by
using only one associativity within infix expressions and making very
few additions atop normal arithmetic notation.

The same character sequences, like `()`, `[]`, and `:` are given
different interpretations depending on their context, whether in a
leader, follower, or line follower position.

Line-expressions are extremely strict on where whitespace may occur
and exactly which kind may be used, because spaces are syntactically
meaningful controls of whether a position is a leader or line follower
position. This does have the advantage of limiting the number of ways
a program is allowed to be format, which may increase the uniformity
of programming styles across the Lexpr community. The biggest
annoyances with this is that it is illegal to add extra spaces to
align things like assignments. That is:

```lexpr
let |   x = 1
    | bar = 2
```
`=>`
```sexpr
(error "unexpected")
```

is illegal because of the extra spaces before `x`.

Comments are a little awkward because they are not allowed anywhere
(i.e. only in line follower positions) and there's no easy way to
comment out an entire line or block of lines, since `#;` requires a
unit position.

It is awkward that blank lines are only okay if they have the correct
indentation. I don't know how to get around this and still allow blank
lines to end blocks without requiring any notation. I think in
practice, it will be insane to use a blank line without sticking in a
`;` so that the fact there are blank lines there is apparent.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

Line-expressions support a familiar syntax with enough flexibility for
interesting language oriented programming. Infix operators are
supported. Familiar function call syntax, as well as references and
templates, is supported. Blocks of statements are
supported. Parentheses can always be added around expressions for
grouping. Text quotations supports embedding plain-text, as well as
facilitates macros that use different parsing rules.

There are too many possible alternative concrete syntaxes to discuss
them here. However, there are some small alternatives worth
mentioning.

It might be possible to replace the infix parsing in the parser with a
macro implementation by parsing groups line `(1 + 2)` into `(#%group
1 + 2)`. The advantage of this is that binding-specific precedence
hierarchies could be defined. It is arguable that this is not an
advantage at all though, because it amplifies the need to understand a
precedence hierarchy that is already awkward in a language like
C. Similarly, it has the advantage of making groups inside of
quotations unparsed. Although it is possible to redefine Lexprs in
this way, I don't think it would be a good idea.

Presently, Lexprs consider sequences (series of groups separated by
commas) as distinct from groups (series of units separated by
spaces). It is conceivable to merge these by adding `,` as an operator
with the loosest precedence. However, with the normal rules of groups,
you would have to have spaces around the commas, `(1 + 2 , 3)`, which
looks horrible. One way to solve this is to allow operators to abut
other units, but this would be very inoperable with existing Racket
programs that use identifiers like
`drracket:language:simple-module-based-language->module-based-language-mixin`. Another
way is to treat `,` as a special operator: the only one that CAN abut
a unit. This is compatible, because existing Racket programs can't
really use `,` in an identifier, because it is an unquote. However,
this shows another problem, which is that `,` at the front is already
an unquote in Lexprs. Overall, this is a very tight space we're in
designing Lexprs, and I think I've done the best you can.

# Prior art
[prior-art]: #prior-art

The prior art document are the root of the repository describes a
variety of related works. Line-expressions are an example of a token
tree.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

# Future possibilities
[future-possibilities]: #future-possibilities

This proposal leaves open the Line-expression shapes expected by
Racket2 core forms and standard library forms. It purposefully leaves
open all questions about the semantic interpretations of most
syntactic structures.

