- Feature Name: Subtle-expressions as fundamental syntax
- Start Date: 2019-08-17
- RFC PR: [racket/racket2-rfcs#109](https://github.com/racket/racket2-rfcs/pull/109)

# Summary
[summary]: #summary

Subtle-expressions are like S-expressions, but with more syntactic
categories and cases. Like S-expressions, they lack a semantic
interpretation. Many existing languages, like C, Java, Haskell, and
Python, are nearly Subtle-expressions.

# Motivation
[motivation]: #motivation

The uniformity of S-expressions limits the amount of information at
the notational level of reading Racket programs. A small amount of
extra notation can go a long way with a small number of mores on its
use. For example, in Racket brackets are used in S-expressions when no
function or macro application is implied (like in the cases of a
`cond`); reading Racket programs without this notational affordance is
more difficult. On the other hand, it is awkward to embed arbitrary
fragments of code not in S-expression format, such as when quoting a
program in another language. The only effective option is to embed a
string. The Racket @-reader is helpful at this, but it is not
uniformly available and the standard structure of Racket's
S-expression based languages do not allow macro-specific reading of
such syntaxes.

Subtle-expressions are an alternative semantics-free tree structure
with more built-in categories and cases that match the styles found in
many other languages, like C and Python. We expect that this notation
will enable language builders in Racket, such as the community for
Racket2, to create many useful mores for writing and reading Racket2
code. Subtle-expressions facilitate the embedding of
non-Subtle-expression syntax via an @-reader-like mechanism.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The reference section goes into great detail about the exact structure
of Subtle-expressions. In this section, we present an example concrete
program with its AST representation inline.

```subtle
let x = 3 * 4 + 5;
// (stx-seq (list let (stx-op = (list x (stx-op + (list (stx-op * (list 3 4)) 5))))))

let p = new Posn(6, 7);
// (stx-seq (list let (stx-op = (list p (stx-seq new Posn (stx-op , (list 6 7)))))))

var a = p.x;
// (stx-seq (list var (stx-op = (list a (stx-op . (list p x))))))

fun f(y, z):
  y + x + z
/* (stx-seq (list fun f (stx-op , (list y z))
     (stx-indent (stx-op + (list y x z))))) */

fun timed_thunk(thunk):
  let before = now();
  let answer = thunk();
  let after = now();
  println "It took" (after - before) "seconds";
  answer
/* (stx-seq (list fun timed_thunk thunk
    (stx-indent (stx-op ; (list
      (stx-seq let (stx-op = (list before (stx-seq now (stx-null)))))
      (stx-seq let (stx-op = (list answer (stx-seq thunk (stx-null)))))
      (stx-seq let (stx-op = (list after (stx-seq now (stx-null)))))
      (stx-seq print-ln (stx-atom "It took") (stx-op - (list after before)) (stx-atom "seconds"))
      answer))))) */

mac timed:
  ( _ e ) => timed_thunk(() => e)
/* (stx-seq (list mac timed
     (stx-indent
       (stx-op => (list
         (stx-seq (list _ e))
         (stx-seq (list timed_thunk (stx-op => (list (stx-null) e))))))))) */

fun g(x, y, (sign : s = +1)):
  x + s * y
/* (stx-seq (list fun g (stx-op , (list x y (stx-op = (list (stx-op : (list sign s) +1)))))
     (stx-indent (stx-op + (list x (stx-op * (list s y))))))) */
    
g(1, 2);
// (stx-seq g (stx-op , (list 1 2)))

g(1, 2, sign: -1);
// (stx-seq g (stx-op , (list 1 2 (stx-op : (list sign -1)))))

dict[key].center.x;
// (stx-op . (list (stx-seq (list dict (stx-group [] key))) center x))

1 + @datalog{ 
  import "nat.log"
  adds(1, X, @(3 + 4))? };
/* (stx-op + (list 1 (stx-txt datalog
     (txt-seq
       "" "  import \"nat.log\""
       "  adds(1, X, "
       (txt-esc (stx-op + (list 3 4)))
       ")?")))) */
```

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

In S-expression-based Racket languages (like `#lang racket`), the body
of module is defined by something like the following grammar:

```
body := form ...

form := atom
      | '(' form ... ')' 
      | '(' form ... '.' form ')'
```

where the parentheses around the cases of `form` may be `()`, `[]`, or
`{}` and `atom` contains many cases described in [The
Reader](https://docs.racket-lang.org/reference/reader.html) chapter of
the reference. (There are other extensions, as well, like the `'` and
`,` abbreviations.) Comments are either line comments (with `;`),
block comments (with `#|` and `|#`), or expression comments (with
`#;`).

This surface syntax is parsed into the AST structure, `stx`, described
as:

```
stx := atom 
     | (stx-cons stx stx)
```

except with additional extensions tracking things like source
location, parenthesis shape, and so on.

Subtle-expressions modify both of these grammars (the surface syntax and
the AST). We start with the AST:

```
stx := (stx-atom atom)
     | (stx-null)
     | (stx-op operator (list stx ...))
     | (stx-group group stx)
     | (stx-seq (list stx ...))
     | (stx-txt stx txt)
     | (stx-indent stx)
     
txt := (txt-esc stx)
     | (txt-str string)
     | (txt-seq (list txt ...))
```

except with additional extensions for things like source location and
so on. Also, `txt-seq`s involving only strings may be collapsed,
except across line breaks.

## Atoms

The concrete syntax of atoms is unspecified at this time.

## Operator expressions 

A sequence like `a op b op c` is parsed as
```
 (stx-op op (list a b c))
```
assuming that `a`, `b`, and `c` do not contain any operators with a
higher level than `op`. For example,
```
  1 + 2 + 3
```
is parsed as
```
  (+ 1 2 3)
```
and
```
  1*3 + 4*7 + 6*9
```
is parsed as
```
  (+ (* 1 3) (* 4 7) (* 6 9))
```
if `*` is a lower level than `+`. But
```
  1 + 3, 4 + 7
```
is parsed as
```
  (, (+ 1 3) (+ 4 7))
```
if `+` is a lower level than `,`.

Operators are the same level in the hierarchy may not be mixed and can
only be written together when combined with grouping or
parentheses. Operators that are "synonymous" (typically the Unicode
glyph for a multi-character operator, like `*` and `×`) are not
considered as different and are allowed together, as well as
collapsed.

The set of operators and their hierarchy is fixed for an instance of a
Subtle-expression language.

As an example, Racket2 could use the following hierarchy order: `::`,
`.`, `→` (synonymous with `->`), `×` (syn with `*`), `÷` (syn with
`/`), `%`, `+`, `-`, all other operators not otherwise mentioned (at
one level), comparison operators (`<`, `≤` (syn with `<=`), `==`, `≠`
(syn with `!=`), etc), `,`, assignment operators (`:=`, `←` (syn with
`<-`), `=`), `⇒` (syn with `=>`), `&`, `|`, and `;`. By "all other
operators", we suggest using a Unicode category and/or a rule like
(identifier sequences that don't include any alphabetic characters.)


## Parenthesized expressions

A sequence like `'(' a b c ')'` is parsed the same as `a b c`, except
that it does not interact with operators surrounding it. This is the
only way to create a `stx-null`.

## Grouped expressions

A sequence like `open a b c close` is parsed as
```
  (stx-group open/close (stx-seq (list a b c)))
```
assuming that `open` and `close` form a matching pair of grouping
characters, like `[]` or `{}` or `⎩⎭`.

The set of grouping characters is fixed for an instance of a
Subtle-expression language. 

As an example, Racket2 could allow `[]`, `{}`, and harvest a Unicode
category for a wider set.

## Sequenced expressions

A sequence like `a b c` is parsed as
```
  (stx-seq (list a b c))
```
assuming that no operators occur inside the sequence.

## Text expressions

A sequence like `@ a { body }` is parsed as
```
  (stx-txt a body)
```
using rules similar to the Racket @-reader. However, the `a` must be a
"single expression", i.e. an `atom` or `group`, i.e. it cannot be an
operator expression or an sequence expression. Inside of `body`, the
`@` character escapes back into a similar "single" context. However,
in a normal context, the `a` may be absent, which is indicated by a
`(stx-null)` in the `a` position; in other words, `@{body}` is
equivalent to `@(){body}`.

## Indented expressions

A sequence like `':' '\n' ' '[K] a '\n' ' '[K] b '\n' ' '[K] c '\n'` (i.e.)
```
  :
  ___a
  ___b
  ___c
```
is parsed as if the `a`, `b`, `c` were in sequence, but wrapped in an
additional `stx-indent` layer; i.e:
```
  (stx-indent (stx-seq (list a b c)))
```
for all K > 1, regardless of the magnitude of K relative to
surrounding lines. If there are intermittent lines with no tokens
other than whitespace, they are ignored. For example,
```
  :
  ___a
  _
  ___b
  _____
  ___c
```
is parsed as
```
  (stx-indent (stx-seq (list a b c)))
```

If a line ends in `:`, then an indented line MUST
follow it. So
```
  a b:
  c
  __d
```

Although in the example, we wrote `:` directly before the `\n`, it is
actually allowed to the left of whitespace and comments.

In situations where there are multiple levels of adjacent indentation,
there will be multiple indent blocks. For example,
```
  :
  ___a:
  _____b
  ___c:
  ______d:
  _______e
  ___f
```
is parsed as
```
  (stx-indent
    (stx-seq
      (list
        a
        (stx-indent b)
        c
        (stx-indent (stx-seq (list d
          (stx-indent e))))
        f)))
```
Notice in this example how `b` and `d` occur with different numbers of
spaces to their left, but this is not observable in the structure of
the resulting syntax object.

Indentation does not interfere with group closing characters. In other
words, group closing characters may appear inside the indentation, or
not. For example,
```
  if a {:
    b }
  else {:
    c }
```
is parsed the same as
```
  if a {:
    b
  } else {:
    c
  }
```
Although only a monster would write the second form.

Similarly, a text expression consumes its own whitespace, which is not
part of any indentation. For example:
```
  if a:
     println @{
     Hello World}
```
is parsed as
```
  (stx-seq (list if a
    (stx-indent (stx-seq (list
      println
      (stx-text #f (txt-str "\n   Hello World")))))))
```

The presence of `stx-indent` allows Subtle-expression languages to
impose indentation sensitivity to particular forms.

We expect that editors will facilitate indentation through the follow
rules:

1. `<Enter>` --- The new line will start at the same indentation level
   as the previous line, unless the previous line ended in a `:`, in
   which case it will be indented one level.
2. `<Tab>` while indentation to the left --- Removes one level of
   indentation (matching the number of spaces in previous lines.)
3. `<Tab>` after reaching column 0 --- Increase level of indentation
   by one until matching the level of indentation of the previous
   line. Then the sequence of `<Tab>`s is complete and we return to
   rule #2.

## Comments

Subtle expressions use `//` for line comments, `/*` & `*/` for block
comments, and `#;` for expression comments, which always consume
a "single" expression, like those that follow `@`.

# Drawbacks
[drawbacks]: #drawbacks

Subtle-expressions are strictly less powerful than a Honu-like
syntax where grouping is binding-sensitive and macros can control how
much of the token stream they read.

Subtle-expressions have rely on an operator precedence hierarchy,
which many people find distasteful. This can be alleviated by having a
very small set of operators which are very easy to remember.

Subtle-expressions have a complicated indentation sensitivity system.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

Subtle-expressions support a familiar syntax with enough flexibility
for interesting language oriented programming. Infix operators are
supported. Familiar function call syntax, as well as references and
templates, is supported. Blocks of statements are
supported. Parentheses can always be added around expressions for
grouping. The `@` supports embedding plain-text, as well as
facilitates macros that use different parsing rules.

There are too many possible alternative concrete syntaxes to discuss
them here. However, there are some small alternatives worth
mentioning.

The proposal makes sense and is useful without the indentation system
at all. However, if it were left out, then two distasteful things
would compensate: (1) groups would be needed more, which would lead to
lines ending like ')}]))]}`, or (2) blocks would be subtly connected
by the precedence rules.

# Prior art
[prior-art]: #prior-art

The prior art document are the root of the repository describes a
variety of related works. Subtle-expressions are an example of a token
tree.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

- What is the syntax of atoms?

- Exactly what operator characters and group characters are
  allowed.
  
- Why is `let x = 3 + 4` not `(= (let x) (+ 3 4))`? Maybe because `=`
  is special and can never have a sequence to its left, i.e. it always
  binds to one expression?

- Why do not need a `;` after a function definition? Maybe because
  operator expressions cannot go inside/beyond indented blocks?
  
- Is worth having unary operators or the ternary operator?
  
# Future possibilities
[future-possibilities]: #future-possibilities

This proposal leaves open the Subtle-expression shapes expected by
Racket2 core forms and standard library forms. It purposefully leaves
open all questions about the semantic interpretations of most
syntactic structures.
