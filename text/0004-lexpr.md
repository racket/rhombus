
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
(ans = 3 * x + y / 4 <= z % 3 && 2 != 5)
```
```sexpr
((#%line (= ans (&& (<= (+ (* 3 x) (/ y 4)) (% z 3))
                    (!= 2 5)))))
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

```lexpr
λ(x, y, def = 5, kw_ext1 => kw_int1, kw_ext2 => kw_int1 = 6, ... rest) :
  {x, y, and kw_ext1 are mandatory}
  {x, y, def, and rest are passed by position}
  {kw_ext1 and kw_ext2 are passed by keyword}
```
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
((#%line (#%dot x (#%dot y z))))
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
{Hello @(#\@)!}
```
```sexpr
((#%line (#%text ("Hello " (#%text-esc #\@) "!"))))
```

```lexpr
{Hello @(newline)!}
```
```sexpr
((#%line (#%text ("Hello " (#%text-esc newline) "!"))))
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
