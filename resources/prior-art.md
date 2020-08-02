# Summary
[summary]: #summary

Relevant prior art. Designs and implementations that will support the language design process.

Note: This is a first draft with entries taken directly from https://github.com/racket/rhombus-brainstroming/issues/14 and racket-users.

# Token Trees
[token trees]: #token-trees

This line of work relies on an idea variously referred to as "skeleton syntax trees", "tree terms", or "token trees". A reader transforms a stream of characters into tokens grouped by balanced delimiters such as braces. The grouping based on delimiters is coarser than in s-expressions; additional parsing remains necessary to separate out different syntactic forms.

Macros receive a sequence of token trees as their input, parse some portion of it, and return the unparsed remainder of the sequence along with their expansion. The output of a macro expansion step is also a token tree.

The following are in rough chronological order.

## Dylan
[Dylan]: #Dylan

* [D-Expressions: Lisp Power, Dylan Style](https://people.csail.mit.edu/jrb/Projects/dexprs.pdf)

## Java Syntactic Extender
[Java Syntactic Extender]: #Java-Syntactic-Extender

* [OOPSALA 2001 paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.83.859&rep=rep1&type=pdf)
* [Talk slides](https://people.eecs.berkeley.edu/~jrb/Projects/oopsla-jse.pdf)

## P4P
[P4P]: #P4P

* [P4P documentation](http://shriram.github.io/p4p/)
* [Shriram on P4P](https://groups.google.com/d/msg/racket-users/ewWuCvbe93k/T_NMS76xAwAJ)

## Honu
[Honu]: #Honu

Compared to the earlier work in this tradition, Honu adds [Top Down Operator Precedence] parsing

* [Honu: Syntactic Extension for Algebraic Notation  through Enforestation](https://www.cs.utah.edu/plt/publications/gpce12-rf.pdf), GPCE 2012
* [Honu documentation](https://docs.racket-lang.org/honu/)

## Star
[Star]: #Star

Star is an independent take on this idea, also adding [Top Down Operator Precedence] parsing

* [Feel Different on the Java Platform: The Star Programming Language](http://www.deinprogramm.de/sperber/papers/star.pdf), PPPJ 2013
* [Star Reference](https://github.com/fgmccabe/StarReference)
* [Star implementation](https://github.com/fgmccabe/star)

## Sweet.js
[Sweet.js]: #sweetjs

* [Sweeten your JavaScript: Hygienic macros for ES5](https://s3.amazonaws.com/academia.edu.documents/35014395/sweetjs.pdf?response-content-disposition=inline%3B%20filename%3DSweeten_Your_JavaScript_-_Hygienic_Macro.pdf&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIWOWYYGZ2Y53UL3A%2F20190723%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20190723T173219Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Signature=49d513920b0d4840d4e4368a867512765757f4e7ed85d78394cc20f032b847b2)
* https://www.sweetjs.org/

## Rust
[Rust]: #Rust

* [Macros section of the Rust book](https://doc.rust-lang.org/1.7.0/book/macros.html)
* [Procedural macro reference](https://github.com/rust-lang-nursery/reference/blob/master/src/procedural-macros.md)


# Alternative reader syntaxes for s-expressions
[reader syntaxes]: #Alternative-reader-syntaxes-for-s-expressions

These approaches provide a reader that produces s-expressions with a similar amount of structure as found in current Racket, but infer some groupings to require fewer parentheses.

## #lang something
[something]: #lang-something

* <https://github.com/tonyg/racket-something>
* [#lang something // infix, optionally indentation-sensitive experimental Racket syntax](https://groups.google.com/d/msg/racket-users/0WyNzafwPCA/jdn4ZqrLCQAJ) - Tony Garnock-Jones 

## Sweet-expressions
[Sweet-expressions]: #Sweet-expressions

* [SRFI 110](https://srfi.schemers.org/srfi-110/srfi-110.html)
* [Sweet](https://docs.racket-lang.org/sweet/) Racket reader
* [Readable Lisp S-expressions Project](https://readable.sourceforge.io/) implemented in Common LISP
* [Sweet.js](https://www.sweetjs.org) - Sweet brings the hygienic macros of languages like Scheme and Rust to JavaScript. 

## Wisp
[Wisp]: #Wisp

* [SRFI 119](https://srfi.schemers.org/srfi-119/srfi-119.html)
* [guile implementation](https://www.draketo.de/english/wisp)

## liso
[liso]: #liso

* <http://breuleux.net/blog/liso.html>

# Grammar composition
[grammar composition]: #Grammar-composition

In these systems languages define grammars, which may extend non-terminals found in other grammars. When languages are used together, the productions of the various grammars are composed to form a single grammar that is used to parse the program. Note that these approaches often require that all language extensions used in a given parse be known up-front in order to form the composed grammar.

* [Parsing Composed Grammars with Language Boxes](https://pdfs.semanticscholar.org/10f8/e0d5590ec702412fcf6dcf4ec6bce402303d.pdf) Section 2, "Parsing Composed Grammars", briefly summarizes some of the challenges with general grammar composition.
* [Parsing Reflective Grammars](http://www.ccs.neu.edu/home/pauls/prg.pdf)
* [Tree Notation Grammar Language](http://jtree.treenotation.org/designer/#standard%20grammar)

## Fortress
[Fortress]: #Fortress

Macros define PEGs, which are composed to determine the parser for a given file

* [Growing a Syntax](https://pdfs.semanticscholar.org/6881/59e0e2964e9500cf51af6c74ca51ff64bb04.pdf)

## SDF in Spoofax, SugarJ etc.
[SDF]: #SDF

SDF is a DSL for defining general context free grammars. When composing grammars, additional rules can be given to resolve ambiguities resulting from the composition.

* [SDF3 Overview](http://www.metaborg.org/en/latest/source/langdev/meta/lang/sdf3/introduction.html)

# elixir-lang
[elixir-lang]: #elixir-lang

Elixir lies somewhere between the [token trees] approach and a fixed grammar. Many forms build their syntax from generic elements. For example, the syntax of the list comprehensions

```
for n <- [1, 2, 3, 4], do: n * n
```

is composed of a call (`for`), an infix operator (`<-`), and a keyword argument (`do:`). `for` is not part of the parser's grammar. However, some forms such as anonymous functions (`fn`) have more specialized syntax built into the parser.

* https://elixir-lang.org/getting-started/meta/macros.html
* [Syntax reference](https://hexdocs.pm/elixir/master/syntax-reference.html#content)
* [Parser implementation](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/src/elixir_parser.yrl)

mflatt commented:
> Mentioned on the mailing list: <https://elixir-lang.org/>

# Pyret
[Pyret]: #Pyret

* [Pyret](https://www.pyret.org)
* <https://www.pyret.org/docs/latest/>
* [Shriram on Pyret](https://groups.google.com/d/msg/racket-users/ewWuCvbe93k/T_NMS76xAwAJ)

# Remix
[Remix]: #Remix

* [Remix](https://docs.racket-lang.org/remix/) - a revised version of Racket
* <https://github.com/jeapostrophe/remix>

# Parinfer
[Parinfer]: #Parinfer

AlexKnauth commented 
> Another thing worth looking at is the Parinfer editor extension, and its line invariant for converting between indentation and paren structure in both directions.

# Mathematica
[Mathematica]: #Mathematica

Mathematica language

> The Mathematica language is a language that uses a non s-expression syntax, but nevertheless feels lispy to use. An application of a function f to arguments x and y is written f[x,y]. This decision makes it easy to use parenthesis for grouping. Using FullForm, TraditionalForm, StandForm and InputForm one can convert between representations of an expression. The full form resembles s-expressions using {} for lists and F[x,y] for applications.
- [soegaard](/soegaard)

* [syntax in Racket](https://docs.racket-lang.org/infix-manual/index.html)
* [The tutorial for Mathematica syntax](https://reference.wolfram.com/language/tutorial/TheSyntaxOfTheWolframLanguage.html)
* [Complete (?) overview of Mathematica syntax](https://reference.wolfram.com/language/guide/Syntax.html)

# Hackett
[Hackett]: #Hackett

LiberalArtist commented:
> @lexi-lambda's Hackett has an infix syntax: <https://github.com/lexi-lambda/hackett/blob/8e4e0e904ac37df58b8c8ef29c0f94ad4151246f/hackett-doc/scribblings/hackett/guide.scrbl#L251> (Link to the Scribble source)

# Julia
[Julia]: #Julia

pschmied commented:
> The Julia language is quite lispy and I believe achieves this in part with a ~Scheme dialect called femtolisp:

* <https://github.com/femtolisp>
* <https://www.julialang.org/>
* <https://docs.julialang.org/en/v1/manual/metaprogramming/index.html>

# list-of-languages-with-s-expression-sugar
[list-of-languages-with-s-expression-sugar]: (#list-of-languages-with-s-expression-sugar)

rocketnia commented:
> The Arc Forum collected a list here a while back: <https://sites.google.com/site/arclanguagewiki/more/list-of-languages-with-s-expression-sugar>

# S-expr
[S-expr]: #S-expr

pschmied commented:
> I don’t know if it’s germane, but some languages have gone the other way—implementing an S-expr surface language atop another language:
* [Erlang / LFE](http://lfe.io/)
* [Python / Hy](http://docs.hylang.org/en/stable/)
* [Lua / Fennel](https://fennel-lang.org/)

# Language workbenches
[language workbenches]: #Language-workbenches

rocketnia commented:
> Another related vein of prior art, which you're no doubt aware of: The idea of "language-oriented programming," especially combined with Python-ish syntax, is something I associate with language workbenches.

* <https://en.wikipedia.org/wiki/Language_workbench>

# Parsing techniques

This section includes references on general parsing techniques. These don't directly address the integration of parsing with macros, but they're relevant to understanding some of the approaches above.

## Top Down Operator Precedence
[Top Down Operator Precedence]: #Top-Down-Operator-Precedence

[Top Down Operator Precedence](http://tdop.github.io) -  Vaughan R. Pratt  Massachusetts Institute of Technology 1973


# PLOT

[Design of the PLOT programming language](http://users.rcn.com/david-moon/PLOT/) by [David Moon](https://en.wikipedia.org/wiki/David_A._Moon).

