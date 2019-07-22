# Summary
[summary]: #summary

Relevant prior art. Designs and implementations that will support the language design process.

Note: This is a first draft with entries taken directly from https://github.com/racket/racket2-rfcs/issues/14 and racket-users.

# Honu
[Honu]: #Honu

* [Honu: Syntactic Extension for Algebraic Notation  through Enforestation](https://www.cs.utah.edu/plt/publications/gpce12-rf.pdf), GPCE 2012
* [Honu documentation](https://docs.racket-lang.org/honu/)

# P4P
[P4P]: #P4P

* [P4P documentation](http://shriram.github.io/p4p/)
* [Shriram on P4P](https://groups.google.com/d/msg/racket-users/ewWuCvbe93k/T_NMS76xAwAJ)

# Pyret
[Pyret]: #Pyret

* [Pyret](https://www.pyret.org)
* <https://www.pyret.org/docs/latest/>
* [Shriram on Pyret](https://groups.google.com/d/msg/racket-users/ewWuCvbe93k/T_NMS76xAwAJ)


# #lang something
[something]: #something
* <https://github.com/tonyg/racket-something>
* [#lang something // infix, optionally indentation-sensitive experimental Racket syntax](https://groups.google.com/d/msg/racket-users/0WyNzafwPCA/jdn4ZqrLCQAJ) - Tony Garnock-Jones 

# Sweet
[Sweet]: #Sweet

* [SRFI 110](https://srfi.schemers.org/srfi-110/srfi-110.html)
* [Sweet](https://docs.racket-lang.org/sweet/) Racket reader
* [Readable Lisp S-expressions Project](https://readable.sourceforge.io/) implemented in Common LISP
* [Sweet.js](https://www.sweetjs.org) - Sweet brings the hygienic macros of languages like Scheme and Rust to JavaScript. 

# Wisp
[Wisp]: #Wisp

* Wisp

# liso
[liso]: #liso

* <http://breuleux.net/blog/liso.html>

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

# list-of-languages-with-s-expression-sugar
[list-of-languages-with-s-expression-sugar]: #list-of-languages-with-s-expression-sugar

rocketnia commented:
> The Arc Forum collected a list here a while back: <https://sites.google.com/site/arclanguagewiki/more/list-of-languages-with-s-expression-sugar>

# Language_workbench
[Language_workbench]: #Language_workbench

rocketnia commented:
> Another related vein of prior art, which you're no doubt aware of: The idea of "language-oriented programming," especially combined with Python-ish syntax, is something I associate with language workbenches.

* <https://en.wikipedia.org/wiki/Language_workbench>

# D-Expressions and The Java Syntactic Extende
[D-Expressions]: #D-Expressions

lexi-lambda commented:
> The papers [D-Expressions: Lisp Power, Dylan Style](https://people.csail.mit.edu/jrb/Projects/dexprs.pdf) and [The Java Syntactic Extender](https://people.eecs.berkeley.edu/~jrb/Projects/oopsla-jse.pdf) from 1999 and 2001, respectively, describe procedural macro systems designed for the Dylan and Java programming languages. Both use a technique very similar to Honu, albeit without the enforestation step that allows programmers to define infix macros.
 
# elixir-lang
[elixir-lang]: #elixir-lang

mflatt commented:
> Mentioned on the mailing list: <https://elixir-lang.org/>
 
# Hackett
[Hackett]: #Hackett

pschmied commented:
> The Julia language is quite lispy and I believe achieves this in part with a ~Scheme dialect called femtolisp:

* <https://github.com/femtolisp>
* <https://www.julialang.org/>
* <https://docs.julialang.org/en/v1/manual/metaprogramming/index.html>

# S-expr
[S-expr]: #S-expr

pschmied commented:
> I don’t know if it’s germane, but some languages have gone the other way—implementing an S-expr surface language atop another language:
* [Erlang / LFE](http://lfe.io/)
* [Python / Hy](http://docs.hylang.org/en/stable/)

# Top Down Operator Precedenc
[Top Down Operator Precedenc]: #Top Down Operator Precedenc

[Top Down Operator Precedence](http://tdop.github.io) -  Vaughan R. Pratt  Massachusetts Institute of Technology 1973




