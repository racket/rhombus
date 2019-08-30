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
traditional infix notation that is parsed using the PEMDASFLTR
(parentheses, exponentiation, multiplication, division, addition,
subtraction, from left to right) algorithm common across the world.

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
`(` which opens a sequence or `.` which precedes another
unit. Sequences are a series of groups separated by commas. Groups are
a series of units separated by spaces and parsed with infix
notation. Lines are a series of units that ends in a
follower. Followers are tokens like a newline, which end a line, or
`&`, which extend a line past a newline, or `:`, which embeds a series
of lines indented one level, or `|`, which embeds a series of lines
aligned with the bar, and so on. Text is delimited by `{` and `}` and
may escape with `@` and is always parsed into lists of characters
split across `\n`.

In general, Lexprs are very strict on their formatting: additional
spaces are never allowed and newlines are meaningful.

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
to use a library of text applications.

## Symbols

Symbols are the most common leaders are any sequence of characters
that is not a number and does not include a character in `\n
.,'()[]{}<>`. As a special case, a symbol may begin with `<` or
include `>` if not inside a parameter application.

```lexpr
x
```
`=>`
```sexpr
((#%line x))
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
characters.

XXX BOUNDARY

XXX Grouping

```lexpr
(x + 6 * y)
```
`=>`
```sexpr
((#%line (+ x (* 6 y))))
```

```lexpr
((x + 6) * y)
```
`=>`
```sexpr
((#%line (* (+ x 6) y)))
```

```lexpr
(ans = 3 * x + y / 4 <= z % 3 && 2 != 5)
```
`=>`
```sexpr
((#%line (= ans (&& (<= (+ (* 3 x) (/ y 4)) (% z 3))
                    (!= 2 5)))))
```

```lexpr
(1 ⊕ 2 + 3)
```
`=>`
```sexpr
((#%line (⊕ 1 (+ 2 3))))
```

```lexpr
(add1 . mult2 $ 5)
```
`=>`
```sexpr
((#%line ($ (|.| add1 mult2) 5)))
```

```lexpr
(sub1 . length . map add1 $ iota 4)
```
`=>`
```sexpr
((#%line ($ (|.| (|.| sub1 length)
                 (#%fun-app map add1))
            (#%fun-app iota 4))))
```

```lexpr
(.)
```
`=>`
```sexpr
((#%line |.|))
```

```lexpr
(1 + 2 <= 3 && false || true)
```
`=>`
```sexpr
(error "Operators with same precedence cannot be used in the same group: || and &&")
```

```lexpr
(1 < 2 == 3)
```
`=>`
```sexpr
(error "Operators with same precedence cannot be used in the same group: == and <")
```

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

XXX Sequences

```lexpr
x y
```
`=>`
```sexpr
((#%line x y))
```

```lexpr
x + y
```
`=>`
```sexpr
((#%line x + y))
```

```lexpr
x + (y + 3)
```
`=>`
```sexpr
((#%line x + (+ y 3)))
```

```lexpr
(x : int)
```
`=>`
```sexpr
((#%line (: x int)))
```

XXX Dots

```lexpr
x.y
```
`=>`
```sexpr
((#%line (#%dot x y)))
```

```lexpr
a ... b
```
`=>`
```sexpr
((#%line a (#%dot |.| |.|) b))
```

```lexpr
x.y.z
```
`=>`
```sexpr
((#%line (#%dot x (#%dot y z))))
```

```lexpr
x.y z
```
`=>`
```sexpr
((#%line (#%dot x y) z))
```

XXX Applications

```lexpr
f(x)
```
`=>`
```sexpr
((#%line (#%fun-app f x)))
```

```lexpr
f(x, y)
```
`=>`
```sexpr
((#%line (#%fun-app f x y)))
```

```lexpr
f(x + 2, y)
```
`=>`
```sexpr
((#%line (#%fun-app f (+ x 2) y)))
```

XXX Member

```lexpr
f[x, y]
```
`=>`
```sexpr
((#%line (#%member f x y)))
```

XXX Param

```lexpr
f<x, y>
```
`=>`
```sexpr
((#%line (#%param f x y)))
```

```lexpr
x < y > z
```
`=>`
```sexpr
((#%line x < y > z))
```

```lexpr
f<A, B>(x, y)[1, 2]
```
`=>`
```sexpr
((#%line (#%member (#%fun-app (#%param f A B) x y) 1 2)))
```

XXX Quotation

```lexpr
(x + '(y * 6) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (* y 6))) z)))
```

```lexpr
x + 'x.y + z
```
`=>`
```sexpr
((#%line x + (#%quote (#%dot x y)) + z))
```

```lexpr
(x + 'f(x) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f x))) z)))
```

```lexpr
(x + 'f(x + ,y) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote y))))) z)))
```

```lexpr
(x + 'f(x + ,g(7, y)) + z)
```
`=>`
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote (#%fun-app g 7 y)))))) z)))
```

XXX Text quotation

```lexpr
{Hello World!}
```
`=>`
```sexpr
((#%line (#%text ("Hello World!"))))
```

```lexpr
{Hello @(1 + 2)!}
```
`=>`
```sexpr
((#%line (#%text ("Hello " (#%text-esc (+ 1 2)) "!"))))
```

```lexpr
{Hello @(#\@)!}
```
`=>`
```sexpr
((#%line (#%text ("Hello " (#%text-esc #\@) "!"))))
```

```lexpr
{Hello @(newline)!}
```
`=>`
```sexpr
((#%line (#%text ("Hello " (#%text-esc newline) "!"))))
```

```lexpr
{Hello

 World!}
```
`=>`
```sexpr
((#%line (#%text ("Hello") () (" World!"))))
```

```lexpr
{}
```
`=>`
```sexpr
((#%line (#%text ())))
```

```lexpr
{@1 + 2!}
```
`=>`
```sexpr
((#%line (#%text ((#%text-esc 1) " + 2!"))))
```

```lexpr
{This is a { embedded brace! } }
```
`=>`
```sexpr
((#%line (#%text ("This is a " "{" " embedded brace! " "}" " "))))
```

```lexpr
let x = item{Some text}
```
`=>`
```sexpr
((#%line let x = (#%text-app item ("Some text"))))
```

XXX Line follower: \n

```lexpr
foo bar
zig zag
```
`=>`
```sexpr
((#%line foo bar) (#%line zig zag))
```

```lexpr
foo bar

zig zag
```
`=>`
```sexpr
((#%line foo bar) (#%line zig zag))
```

XXX Line follower: \

```lexpr
foo \
  bar
zig zag
```
`=>`
```sexpr
((#%line foo bar) (#%line zig zag))
```

```lexpr
foo \
  bar baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

```lexpr
foo \
  bar baz
zig zag
```
`=>`
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

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

XXX Line follower: :

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

```lexpr
zig :
  zag
  zog
```
`=>`
```sexpr
((#%line zig (#%indent (#%line zag) (#%line zog))))
```

```lexpr
zig : zag
  zog
```
`=>`
```sexpr
((#%line zig (#%indent (#%line zag) (#%line zog))))
```

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

XXX MORE sacred cows: no named characters (these are just variables
that you can use `@(BLAH)`) to get ; limited numbers and other
literals (I don't like literal vectors, etc, and I think things like
`binary{01010101}` are a better interface to new notations. In
particular, they are more uniform than having just a few things built
in.)

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

XXX <> is ugly special casing
