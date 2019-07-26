- Feature Name: C-expressions as fundamental syntax
- Start Date: 2019-07-25
- RFC PR: [racket/racket2-rfcs#88](https://github.com/racket/racket2-rfcs/pull/88)

# Summary
[summary]: #summary

C-expressions are like S-expressions, but with more syntactic
categories and cases. Like S-expressions, they lack a semantic
interpretation. Many existing languages, like C and Java, are nearly
C-expressions.

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

C-expressions are an alternative semantics-free tree structure with
more built-in categories and cases that match the styles found in many
other languages, like C. We expect that this notation will enable
language builders in Racket, such as the community for Racket2, to
create many useful mores for writing and reading Racket2
code. C-expressions facilitate the embedding of non-C-expression
syntax via an @-reader-like mechanism.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The reference section goes into great detail about the exact structure
of C-expressions. In this section, we present an example concrete
program with its AST representation inline.

```
var x = 3;
// (stx-stmt var (stx-op = x 3))

fun f(y, z) { 
 y + x + z; }
/* (stx-stmt fun (stx-app PAREN f (stx-op , y z))
    (stx-stmts (stx-stmt (stx-op + y x z)))) */

let q = x + 1;
// (stx-stmt let (stx-op = q (stx-op + x 1)))

fun timed_thunk(thunk) {
 let before = now();
 let answer = thunk();
 let after = now();
 println "It took" (after - before) "seconds";
 answer; }
/* (stx-stmt fun (stx-app PAREN timed thunk)
    (stx-stmts
     (stx-stmt let (stx-op = before (stx-app PAREN now null)))
     (stx-stmt let (stx-op = answer (stx-app PAREN thunk null)))
     (stx-stmt let (stx-op = after (stx-app PAREN now null)))
     (stx-stmt println "It took" (stx-op - after before) "seconds")
     (stx-stmt answer))) */
     
mac timed {
 case { _ e; } => timed_thunk(() => e);
 case _(e:expr) => { timed e; } }
/* (stx-stmt mac timed
    (stx-stmts
     (stx-stmt case (stx-op => (stx-stmts (stx-stmt _ e)) (stx-app PAREN timed_thunk (stx-op => null e))))
     (stx-stmt case (stx-op => (stx-app PAREN e (stx-op : e expr)) (stx-stmts (stx-stmt timed e)))))) */

fun g(x, y, (sign : s = +1)) {
 x + (s * y); }
/* (stx-stmt fun (stx-app PAREN g (stx-op , x y (stx-op = (stx-op : sign s) +1)))
    (stx-stmts (stx-stmt (stx-op + x (stx-op * s y))))) */
    
g(1, 2);
// (stx-stmt (stx-app PAREN g 1 2))

g(1, 2, sign: -1);
// (stx-stmt (stx-app PAREN g 1 2 (stx-op : sign -1)))

dict[key].center.x;
// (stx-stmt (stx-op (stx-op . (stx-app BRACKET dict key) center) x))

1 + @datalog{ 
 import "nat.log"
 adds(1, X, @(3 + 4))? };
/* (stx-stmt (stx-op + 1
    (stx-at datalog
     (stx-txt-seq "" "import \"nat.log\"" 
      (stx-txt-seq
        "adds(1, X, " 
        (stx-txt-at (stx-op + 3 4))
        ")?"))))) */
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

C-expressions modify both of these grammars (the surface syntax and
the AST). We start with the AST:

```
stx := atom
     | (stx-stmts stx ...)
     | (stx-stmt stx ...+)
     | (stx-op op stx ...+)
     | (stx-app shape stx stx)
     | (stx-at stx txt)
     
txt := (stx-txt-at stx)
     | string
     | (stx-txt-seq txt ...)
```

except with additional extensions for things like source location and
so on. Also, `stx-txt-seq`s involving only strings may be collapsed,
except across line breaks.

The body of a C-expression-based module is defined by the following
grammar. We annotate the grammar with the AST produced.

```
body := cs ... --- (stx-stmts $0 ...)

cs := ce ...+ ';' --- (stx-stmt $0 ...)
       
ce := atom                     --- $0
    | `op`                     --- $0 (the atom)
    | op ce                    --- (stx-op $0 $1)
    | ce op ce                 --- (stx-op $1 $0 $2)
    | ce `symbol` ce           --- (stx-op $1 $0 $2)
    | ce sho ce shc            --- (stx-app $1 $0 $2)
    | '{' body '}'             --- $1
    | '(' ce ')'               --- $1
    | '@' ce '{' text-body '}' --- (stx-at $1 $3)
    |                          --- null (an atom)

text-body := '@' ce            --- (stx-txt-at $1)
          | '{' text-body '}'  --- (stx-txt-seq $0 $1 $2)
          | any-char text-body --- (stx-txt-seq $0 $1)
```

where `atom` contains many concrete values (like with S-expressions),
`sho` (`shc`) is an open (close) shape character that match, `op` is
any sequence of operator characters, and `any-char` is any character
(except `@`, `{`, or `}`.) Shape character pairs are either `()`,
`[]`, `<>`, or other Unicode pairs except `{}`. An operator character
is something like the Unicode category Sm or non-alphanumeric
characters not otherwise mentioned in the grammar. Comments are either
line comments (with `//`), block comments (with `/*` and `*/`), or
expression comments (with `#;`).

Infix operator parsing has some special constraints. Operators are
divided into three categories: "loose", "tight", and "other". Loose
operators have the lowest precedence, cannot be mixed at their level,
and are flattened into a single `stx-op` node. Tight operators have
the highest precedence, can be mixed at their level, and associate to
the left. All other operators have middle precedence, cannot be mixed
at their level, and are flattened into a single `stx-op` node. We
propose that loose operators are `,` & '=' & '=>' & '&&' & '||' and
tight operators are `.` & `->` & `:` & `::`. Here are some example
parses:

(Note: I am not confident that this paragraph mandates these
parses. If it doesn't, the explanation has the error, not the parses.)

```
1 + 2                 // --> (stx-op + 1 2)
1 + 2 + 3             // --> (stx-op + 1 2 3)
p.x + p.y             // --> (stx-op + (stx-op . p x) (stx-op . p y))
s->p1.x + p2.y, p3::z // --> (stx-op , (stx-op + (stx-op . (stx-op -> s p1) x) (stx-op . p2 y)) (stx-op :: p3 z))
1 + 2 * 3             // --> illegal
```

In a `cs` sequence described above, a token stream like `ce op ce` is
ambiguous, because it could be `ce`, `op ce` (two expressions with one
unary) or `ce op ce` (one expression with one binary). The grammar
above does not say which it is. We mandate that the second
interpretation holds. Similarly `ce op1 ce op2 ce` is either illegal
(assuming the operations are all "other") or `ce op1 ce` and `op2 ce`
(two expressions, a binary and a unary). We mandate the second
interpretation.

# Drawbacks
[drawbacks]: #drawbacks

C-expressions are strictly less powerful than a Honu-like
syntax where macros are free to read an arbitrary number of tokens
into the token stream. (C-expressions mandate that a macro stop at a
`;` in a stream.)

The various kinds of operators is a little complicated and it is a
draw back to not support arbitrary precedence relations. This is an
attempt to be simple and not require users to understand precedence
relations, while supporting the most common kinds of mixed
operations. 

The ambiguity of infix operations in `cs` is awkward.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

C-expressions support a familiar syntax with enough flexibility for
interesting language oriented programming. Infix operators without
confusing precedence rules are supported. Familiar function call
syntax, as well as references and templates, is supported. Blocks of
statements are supported. Parentheses can always be added around
expressions for grouping. The `@` supports embedding plain-text, as
well as macros that use different parsing rules.

There are too many possible alternative concrete syntaxes to discuss
them here. However, there are some small alternatives worth
mentioning.

The C-expression AST could be simplified by mapping `(stx-at stx txt)`
to `(stx-app @ stx txt)`, `(stx-op op stx ...+)` to `(stx-app op (list
stx ...+)`), `(stx-stmt stx ...+)` to `(stx-app ; (list stx ...+))`
and `(stx-stmts stx ...)` to `(stx-app body (list stx ...))`, thus
unifying all the variants of syntax ASTs into a single shape.

# Prior art
[prior-art]: #prior-art

The prior art document are the root of the repository describes a
variety of related works. C-expressions are an example of a token tree
and they use the Pyret rule about not mixing infix operators.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

- What is the syntax of atoms?

- Exactly what operator characters and shape characters are
  allowed. Ideally, they would be specified in a simple and broad way,
  such as by a Unicode category.
  
- Is the C-expression grammar efficient to parse? Are the constraints
  in it sufficient for deterministic parsing? Is the explanation of
  parsing above consistent with the desired example parses?

- The expression comment sequence `#;` doesn't fit with the other
  comment characters.

# Future possibilities
[future-possibilities]: #future-possibilities

This proposal leaves open the C-expression shapes expected by Racket2
core forms and standard library forms. It purposefully leaves open all
questions about the semantic interpretations of most structures.
