
XXX Numbers

```lexpr
1
```
```sexpr
((#%line 1))
```

XXX Symbols

```lexpr
x
```
```sexpr
((#%line x))
```

XXX Characters

```lexpr
#\

```
```sexpr
((#%line #\newline))
```

```lexpr
#\a
```
```sexpr
((#%line #\a))
```

XXX Grouping

```lexpr
(x + 6 * y)
```
```sexpr
((#%line (+ x (* 6 y))))
```

```lexpr
((x + 6) * y)
```
```sexpr
((#%line (* (+ x 6) y)))
```

```lexpr
(3 * x + y / 4 <= z % 3 && 2 != 5)
```
```sexpr
((#%line (&& (<= (+ (* 3 x) (/ y 4)) (% z 3))
             (!= 2 5))))
```

```lexpr
(1 ⊕ 2 + 3)
```
```sexpr
((#%line (⊕ 1 (+ 2 3))))
```

```lexpr
(add1 . mult2 $ 5)
```
```sexpr
((#%line ($ (|.| add1 mult2) 5)))
```

```lexpr
(sub1 . length . map add1 $ iota 4)
```
```sexpr
((#%line ($ (|.| (|.| sub1 length)
                 (#%fun-app map add1))
            (#%fun-app iota 4))))
```

```lexpr
(.)
```
```sexpr
((#%line |.|))
```

```lexpr
(1 + 2 <= 3 && false || true)
```
```sexpr
(error "Operators with same precedence cannot be used in the same group: || and &&")
```

```lexpr
(1 < 2 == 3)
```
```sexpr
(error "Operators with same precedence cannot be used in the same group: == and <")
```

XXX Sequences

```lexpr
x y
```
```sexpr
((#%line x y))
```

```lexpr
x + y
```
```sexpr
((#%line x + y))
```

```lexpr
x + (y + 3)
```
```sexpr
((#%line x + (+ y 3)))
```

```lexpr
(x : int)
```
```sexpr
((#%line (: x int)))
```

XXX Dots

```lexpr
x.y
```
```sexpr
((#%line (#%dot x y)))
```

```lexpr
a ... b
```
```sexpr
((#%line a (#%dot |.| |.|) b))
```

```lexpr
x.y.z
```
```sexpr
((#%line (#%dot x y z)))
```

```lexpr
x.y z
```
```sexpr
((#%line (#%dot x y) z))
```

XXX Applications

```lexpr
f(x)
```
```sexpr
((#%line (#%fun-app f x)))
```

```lexpr
f(x, y)
```
```sexpr
((#%line (#%fun-app f x y)))
```

```lexpr
f(x + 2, y)
```
```sexpr
((#%line (#%fun-app f (+ x 2) y)))
```

XXX Member

```lexpr
f[x, y]
```
```sexpr
((#%line (#%member f x y)))
```

XXX Param

```lexpr
f<x, y>
```
```sexpr
((#%line (#%param f x y)))
```

```lexpr
x < y > z
```
```sexpr
((#%line x < y > z))
```

```lexpr
f<A, B>(x, y)[1, 2]
```
```sexpr
((#%line (#%member (#%fun-app (#%param f A B) x y) 1 2)))
```

XXX Quotation

```lexpr
(x + '(y * 6) + z)
```
```sexpr
((#%line (+ (+ x (#%quote (* y 6))) z)))
```

```lexpr
x + 'x.y + z
```
```sexpr
((#%line x + (#%quote (#%dot x y)) + z))
```

```lexpr
(x + 'f(x) + z)
```
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f x))) z)))
```

```lexpr
(x + 'f(x + ,y) + z)
```
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote y))))) z)))
```

```lexpr
(x + 'f(x + ,g(7, y)) + z)
```
```sexpr
((#%line (+ (+ x (#%quote (#%fun-app f (+ x (#%unquote (#%fun-app g 7 y)))))) z)))
```

XXX Text quotation

```lexpr
{Hello World!}
```
```sexpr
((#%line (#%text ("Hello World!"))))
```

```lexpr
{Hello @(1 + 2)!}
```
```sexpr
((#%line (#%text ("Hello " (#%text-esc (+ 1 2)) "!"))))
```

```lexpr
{Hello

 World!}
```
```sexpr
((#%line (#%text ("Hello") () (" World!"))))
```

```lexpr
{}
```
```sexpr
((#%line (#%text ())))
```

```lexpr
{@1 + 2!}
```
```sexpr
((#%line (#%text ((#%text-esc 1) " + 2!"))))
```

```lexpr
{This is a { embedded brace! } }
```
```sexpr
((#%line (#%text ("This is a " "{" " embedded brace! " "}" " "))))
```

```lexpr
let x = item{Some text}
```
```sexpr
((#%line let x = (#%text-app item ("Some text"))))
```

XXX Line follower: \n

```lexpr
foo bar
zig zag
```
```sexpr
((#%line foo bar) (#%line zig zag))
```

```lexpr
foo bar

zig zag
```
```sexpr
((#%line foo bar) (#%line zig zag))
```

XXX Line follower: \

```lexpr
foo \
  bar
zig zag
```
```sexpr
((#%line foo bar) (#%line zig zag))
```

```lexpr
foo \
  bar baz
zig zag
```
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

```lexpr
foo \
  bar baz
zig zag
```
```sexpr
((#%line foo bar baz) (#%line zig zag))
```

```lexpr
foo \
  bar \
    baz
zig zag
```
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
```sexpr
((#%line let (#%bar (#%line x = 1) (#%line y = 2)) in (#%indent (#%line x + y))))
```

```lexpr
fun sum(l) :
  match l \
    | empty :
        0
    | cons(a, d) :
        (a + sum(d))
```
```sexpr
((#%line fun (#%fun-app sum l)
  (#%indent
    (#%line match l
      (#%bar
        (#%line empty 
         (#%indent (#%line 0)))
        (#%line (#%fun-app cons a d)
         (#%indent (#%line (+ a (#%fun-app sum d))))))))))
```

XXX Embedded lines

```lexpr
foo bar [zig zag]
```
```sexpr
((#%line foo bar (#%line zig zag)))
```

```lexpr
foo bar [zig [baz] zag]
```
```sexpr
((#%line foo bar (#%line zig (#%line baz) zag)))
```

```lexpr
foo bar [zig \
           zag] baz
```
```sexpr
((#%line foo bar (#%line zig zag) baz))
```

```lexpr
foo bar [zig :
           zag
           zog] baz
```
```sexpr
((#%line foo bar (#%line zig (#%indent (#%line zag) (#%line zog))) baz))
```

XXX sacred cows: no strings or named characters; limited numbers and
other literals

XXX goal: only one way to format
--- not really, because of \ and precedence

XXX sacred cow: comments

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

XXX BOUNDARY

Line expressions (`lexpr`) are primarily line-oriented, but they embed
two other kinds of expressions: individual and sequence expressions,
both which are made up of atomic expressions.

White space is a sequence of space characters and comments. Comments
are either line oriented with `//`, block oriented with `/*` and
`*/`, or expressions which start with `#;` and consume an individual
expression.

An atomic expression is a single base object, like a symbol or
number.

An operator is a sequence of special operator characters.

A text expression is like the Racket @-reader, except that it is
indexed by an ignored prefix of characters, typically a sequence of
spaces.

Individual expressions (`iexpr`) are white-space-free prefixes of
characters (modulo comments), which may embed sequence
expressions. They can also include text expressions with `{`

```bnf
iexpr := atom
       | operator
       | iexpr '.' atom
       | iexpr '(' qexpr ')'
       | iexpr '[' qexpr ']'
       | iexpr '<' qexpr '>'
       | '(' qexpr ')'
       | '{' text[ϵ] '}'
```

XXX Single (`'`) and line (`[]`) quotations

Sequence expressions (`qexpr`) are sequences of individual expression
separated by whitespace. Operators effect the parsing of sequence
expressions.

```bnf
qexpr  := ϵ
        | iexpr q*expr

q*expr := ϵ
        | WS qexpr
```

Line expressions are indexed by a prefix which they must present and a
prefix that must occur in embedded expressions. They are a sequence of
this prefix, an individual expression, a sequence expression, and a
line tail. The body of a module is parsed as a sequence of line
expressions with no prefix.

```bnf
lexpr[pre, tailpre] := pre iexpr q*expr ltail[tailpre]
```

There are a variety of line tails:

A newline, which terminates a line expression.

```bnf
ltail[tailpre] := .... | '\n'
```

A `:`, which visibly embeds a sequence of line expressions at one
higher level of indentation, followed by an optional line expression
at the same level of indentation.

```bnf
ltail[tailpre] := .... | ':' WS* '\n' lexpr[tailpre SP SP, tailpre] * lexpr[tailpre, tailpre]?
```

For example,

```lexpr1
if x < y :
  "Left"
else :
  "Right"
```

is parsed as

```sexpr
(if (x < y) (: ("Left")) else (: ("Right")))
```

A `&`, which invisibly embeds a sequence of line expressions at the
same level of indentation.

```bnf
ltail[tailpre] := .... | '&' WS* '\n' lexpr[tailpre, tailpre] *
```

For example,

```lexpr1
begin &
a
b
c
```

is parsed as

```sexpr
(begin (a) (b) (c))
```

A `\`, which invisibly continues the line expression at one
high level of indentation.

XXX update BNF

```bnf
ltail[tailpre] := .... | '\' WS* '\n' lexpr[pre SP SP, tailpre] *
```

For example,

```lexpr1
begin \
  a
  b
  c
```

is parsed as

```sexpr
(begin a b c)
```

A `|`, which visibly embeds a sequence of line expressions with
aligned `|`s. (The notation in the following BNF is lacking.)

```bnf
ltail[tailpre] := .... | '|'@col lexpr[ϵ, tailpre']? lexpr[pre, tailpre'] *
  where      pre = SP{col-1} '|'
        tailpre' = SP{col}
```

For example,

```lexpr1
data List | Empty
          | Cons(a, b)
```

is

```sexpr
(data List (#%bar (Empty) (#%app Cons (#%comma a b))))
```

and

```lexpr1
define length(l) :
  match l with \
    | Empty => 0
    | Cons(a, b) => 1 + length(b)
```

is parsed as

```sexpr
(define (#%app length (l))
  (: (match l with 
       (#%bar (Empty => 0)
          ((#%app Cons (#%comma a b)) => 1 + (#%app length b))))))
```

A `@`, which embeds an indented text block.

```bnf
ltail[tailpre] := .... | '@' WS* '\n' text[tailpre]
```

For example,

```lexpr1
datalog @
  import "family.log"
  add(X, @5, Y)?
```

is parsed as

```sexpr
(datalog (#%text (list (list "import \"family.log\"")
                       (list "add(X, " (#%text-esc 5) ", Y)?"))))
```

A `@{`, which embeds an indented text block that is
terminated with `}` and continues the line expression.

```bnf
ltail[tailpre] := .... | '@{' WS* '\n' text[tailpre] `}` lexpr[tailpre, tailpre]
```

For example,

```lexpr1
c @{
  double log2(double x) {
   return log(x) / log(2); }
} with \
  "-lmath"
```

parses as

```sexpr
(c (#%text (list (list "double log2(double x) {")
                 (list " return log(x) / log(2); }")))
   with "-lmath")
```

XXX lots of overlap with `#lang something`: https://github.com/tonyg/racket-something
