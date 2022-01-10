State of Rhombus - DRAFT

Jack Firth, October 2021

# Motivation

Today, Racket promises language-oriented programming to its users. But
this promise has a constraint: if you want to build a macro-extensible
language, your language has to use S-expressions. This constraint is
unfairly limiting, as telling users “you can build any extensible
language you want, as long as it uses S-expressions” is akin to Henry
Ford telling Model-T buyers “your car can be any color you want, as
long as it’s black.” We’d like to lift this limitation with a new
programming language while taking this opportunity to improve Racket
in backward-incompatible ways.

# Overview

Rhombus is an experiment in building a **macro-extensible**
general-purpose programming language with **conventional surface syntax**
and improved libraries on top of **the Racket platform**.

# Details

By “macro-extensible,” we mean that Rhombus users should be able to
extend the syntax of Rhombus in much the same way that Racket users
can extend the syntax of Racket. Complex syntactic extensions such as
object and class systems, static type checkers, and pattern matching
should be implemented as libraries while still providing a surface
syntax familiar to users of these features in other languages.

By “conventional surface syntax,” we mean conventional to both users
of widely-used programming languages as well as newcomers who are
familiar with basic math notation. This leads us to the following
constraints:

 * Arithmetic is infix, for example, `1+x`
 
 * Function calls are written as a function expression followed by a
   parenthesized comma-separated list of arguments, such as `f(x, y, z)`
 
 * Extra parentheses can be used freely to group things and
   disambiguate, such as `(1 + 2) * 3`, without introducing new
   structures or semantics.
 
 * Dots are used for member selection, like `point.x`
 
 * Syntactic forms are connected by adjacency rather than in a strict
   hierarchy. For example `do { } while ( )` and `x where y = z`. This
   is a kind of “infix” that is broader than just arithmetic.

By “improved libraries,” we mean that Rhombus should not be beholden
to the backward compatibility constraints of Racket. Rhombus should be
free to innovate in providing new standard libraries for features that
are becoming commonplace in other languages, such as generic
collections, stream processing, and asynchronous programming.

By “on top of the Racket platform,” we mean that Rhombus should be a
`#lang` like any other, and it should be possible for users to combine
Rhombus code and libraries with ordinary Racket code and libraries
seamlessly. Where we are now

A few surface syntax proposals have been brainstormed, and the
proposals fit into two broad categories:

 * **C-like** surface syntaxes, which use **braces** and semicolons**
   to convey general structure. Jay’s
   [C-expressions](https://github.com/racket/rhombus-brainstorming/pull/88)
   fit this category.

 * **Python-like** surface syntaxes, which use **colons** and
   **indentation** to convey general structure. Jay’s
   [lexprs](https://github.com/racket/rhombus-brainstorming/pull/114) and
   Matthew’s
   [shrubbery notation](https://github.com/racket/rhombus-brainstorming/pull/122)
   fit this category.

Separately, there’s the question of how much syntactic grouping the
surface syntax should be responsible for versus how much grouping is
left up to the macro system. Three options are possible:

 * The surface syntax is responsible for **all** grouping. This is roughly
   where traditional S-expressions lie, and it’s awkward enough that
   many Lisps relax the S-expressions-for-everything constraint and
   let macros create implicit groupings. For example, Clojure’s `let`
   form implicitly groups binding-expression pairs based on adjacency,
   as in `(let [x 1 y 2] (+ x y))`. Racket’s keyword arguments are a
   similar instance of relaxing this constraint.
   [C-expressions](https://github.com/racket/rhombus-brainstorming/pull/88) are an
   attempt to have “conventional surface syntax” with this property.
 
 * The surface syntax is responsible for **no** grouping. This results in
   a Forth-like syntax where the program is just a flat sequence of
   tokens. This doesn’t satisfy our “conventional surface syntax”
   requirement.
 
 * The surface syntax is responsible for **some** grouping, but not all.
   This requires that the parser understand the program’s bindings and
   expand macros to get a complete picture of a program’s structure.
   Interleaving the parsing of a token stream with macro expansion in
   this fashion is called enforestation, and it allows macros to
   extend the syntax of conventional languages. The
   [Honu](https://www.cs.utah.edu/plt/publications/gpce12-rf.pdf) research
   language pioneered this technique.

# What’s Next

We’ve decided on a **Python-like** surface syntax **with
enforestation**, specifically shrubbery notation, as the first
proposal to advance to the next prototyping stage. This may not be the
final direction that the Rhombus project takes, but it’s the direction
we’re prioritizing for the time being. We made this decision based on
the following factors:

 * Indentation sensitivity a la Python has the advantage that the
   visual grouping on the screen matches grouping for parsing, but it
   also leaves plenty of room for operator- and identifier-driven
   context-sensitive parsing.

 * Matthew tried several experiments and found Python-style notation
   most straightforward to work with, and he’s been the primary
   Rhombus implementor thus far.

Making the “correct” decision about surface syntax isn’t our goal at
this stage. We’re still firmly in the prototyping phase of the Rhombus
project. Our goal is to stake out territory in the design space and
get a deeper understanding of what challenges lie ahead. For that
reason, **we plan to build a language around shrubbery notation**
next. We don’t believe that shrubbery notation is perfect. Still, we
believe that the understanding gained from building a working
programming language with shrubbery notation will help us further
refine shrubbery notation. It will also be vital in determining what
other surface syntaxes are worth exploring.

To build a language, **we need to write libraries**. The libraries we can
work on fall into two broad categories: general-purpose and
special-purpose. General-purpose libraries are necessary to define the
APIs of many other libraries, such as a class system, concurrency
abstractions, or generic collections. Special-purpose libraries are
specific to a narrow (but practically useful) domain, such as text
parsing or theorem proving. Special purpose libraries leverage
domain-specific languages more heavily, so they’ll give us a good
sense of how well shrubbery notation can cooperate with complex DSLs.

**Libraries are an excellent place for new Rhombus contributors to jump
in!** We’re still in the early stages, so exploring multiple approaches
to the same problem domain is valuable. There’s a wide variety of
directions to explore. Below are some we’re already considering to
varying degrees, but we encourage additional proposals.

## Generic collections

General purpose. Volunteers:
[@jackfirth](https://github.com/jackfirth/). Relevant GitHub issues:
[#19](https://github.com/racket/rhombus-brainstorming/issues/19),
[#21](https://github.com/racket/rhombus-brainstorming/issues/21),
[#28](https://github.com/racket/rhombus-brainstorming/issues/28),
[#80](https://github.com/racket/rhombus-brainstorming/issues/80),
[#102](https://github.com/racket/rhombus-brainstorming/issues/102),
[#147](https://github.com/racket/rhombus-brainstorming/issues/147).

Rhombus should provide common data structures for generic collections,
such as lists, sets, maps, and the tools to abstract over different
kinds of collections. Racket’s monomorphic approach to collections has
been a [long-standing complaint](http://programming-puzzler.blogspot.com/2010/08/racket-vs-clojure.html), and it’s one of the few areas the
community almost universally agrees needs improvement.

## Classes and structs

General purpose. Volunteers: none yet. Relevant GitHub issues:
[#57](https://github.com/racket/rhombus-brainstorming/issues/57),
[#76](https://github.com/racket/rhombus-brainstorming/issues/176),
[#151](https://github.com/racket/rhombus-brainstorming/issues/151),
[#152](https://github.com/racket/rhombus-brainstorming/issues/152).

Rhombus should provide a better, more unified system for defining new
data types and interfaces than Racket’s current mishmash of structs,
classes, generics, and structure type properties. There’s a lot of
room to leverage shrubbery notation here, especially if we were to
build some sort of [dot transformer protocol](https://github.com/racket/rhombus-brainstorming/issues/151) for macros to use.

## Other ideas

* Concurrency and parallelism. Can we do better than Racket here?

  - [@jackfirth](https://github.com/jackfirth/) - Software
    transactional memory à la Clojure would be convenient.

* Annotations, types, and contracts. Racket’s runtime contracts and
  Typed Racket’s static types are at two extreme ends of the
  dynamic-static correctness spectrum. Can Rhombus occupy a more
  user-friendly middle ground?

* Multi-language Scribble. If we want Rhombus and Racket libraries to
  be seamlessly interoperable, we need a way for Scribble to translate
  library documentation for different surface syntaxes. In-source
  documentation might be worth exploring here too.

* Redex, but better.


# Appendices

## Rhombus Plan
```
---------
From: Matthew Flatt <Unknown>
Date: Wednesday, October 2, 2019 at 8:27:50 PM UTC+1
Subject: Rhombus project plan
To: Racket-Users List <Unknown>


[[NOTE: "Rhombus" is the new name for a design project formerly known 
as "Racket2", but "Rhombus" IS NOT THE FINAL NAME OF THE NEW LANGUAGE. 

"Rhombus" is the name of the project that will develop a language, 
and "Rhombus" is a temporary stand-in for a language name to be 
determined later. Phase 3 of the plan includes the mandatory step of 
picking a new language name.]] 

Rhombus is about building on the good parts of Racket and advancing the 
frontier of Racket-style language-oriented programming. A significant 
part of the experiment is trying a surface syntax other than 
parenthesized prefix notation. Another part is simplifying and 
generalizing elements of `#lang racket`, such as its data structures 
for streams and binding with integrated and extensible 
pattern-matching. While some of these goals could be pursued 
independently, taking them together offers opportunities to make the 
whole language fit together better. 

Whether Rhombus will become a suitable alternative for current `#lang 
racket` can be determined only as the experiment progresses. It starts 
with that ambition, but the project may fail. It may fail for technical 
reasons, process reasons, or social reasons: 

- On the technical side, we're trying to do something new. 

- On the process side, we are trying a more visible and open approach 
than we have used for past major changes, even to the point of 
opening up the early exploratory phase. 

- On the social side, we hope that skeptical Racketeers will make room 
for the experiment and understand that putting the experiment in the 
open (and being up-front about development costs) is part of the 
more open process. 

Matthew Flatt will lead the project with the guidance and consent of 
Racket project leadership. In early phases of the experiment, Matthew 
is responsible for delegating and deciding when the next phase starts. 
Toward the end of the process, Racket leadership is responsible for 
deciding whether to continue. Community participation is built into the 
process by keeping the design discussion open and by using an RFC 
process for the eventual design elements. 


What Will Happen to Racket During Rhombus 
----------------------------------------- 

The Racket team will continue to maintain the language and its 
implementations: 

- The existing ecosystem will continue to function as always. 

- Existing `#lang racket` programs will continue to run, just as in 
the 6.x and 7.x series of releases. 

- The team will release updated versions, occasionally making modest 
incompatibilities with explicit transition paths as needed --- all 
as usual. 

This does not mean that the language and its implementation will evolve 
at the same speed as it has in the past, but it means that we will 
maintain our standard commitment to reliability and quality. 


Phase 1: Brainstorming (months) 
---------------------- 

GOAL AND OUTPUT: A design sketch and collection of prototype 
implementations that reflect key ideas and design constraints. 

PROCESS: This is the current phase --- a discussion of ideas and 
potential directions at 

https://github.com/racket/rhombus-brainstorming 
[formerly "racket2-rfcs"] 

There will be some implementation in this phase to try things out, but 
at first only for exploration purposes. 

Initially, we want to address 

- generality in the data structures and libraries, 

- consistency in the binding names and terminology, and 

- a surface syntax other than parenthesized-prefix notation. 

We also presuppose a potential transition from `#lang racket`, which 
will constrain the space of plausible languages. Depending on how this 
phase unfolds, we are willing to consider the addition of goals, their 
removal, or their reformulation. 

This process will take a while, because the space is very large, 
because different participants in the discussion will start with one 
set of opinions and end with different ones, and because all of this 
brainstorming and exploration will be publicly visible. 

Some draft proposals using the RFC template will be useful at this 
phase, similar to prototype implementations, but the process will be 
informal (so, not really an RFC process). The existing "Racket2 wish 
list" is also part of this phase, but some effort will be needed to 
select, consolidate, and elaborate wish-list items. 

CONCLUSION: The project leader will decide on the point where there's 
enough agreement on specific design constraints and the outline of a 
design to move to the next phase. 

Failure is an option; some of the original goals may be abandoned, and 
the project as a whole may be abandoned if the project leader cannot 
identify a suitable product to move on to the next phase. 

The project leader will also use this first process to identify 
contributors and working-group leaders for the second phase. 


Phase 2: Iterative Design (years) 
------------------------- 

GOAL AND OUTPUT: Specification and a coherent prototype for the overall 
language design, stable enough for practical work and at the same time 
subject to refinement through practice. 

PROCESS: This phase starts work on an implementation that is intended 
to last, consolidating ideas that emerged from the brainstorming phase 
and exposing how well different ideas fit together at scale. The design 
will evolve in response to the implementation effort, but it should 
eventually converge. 

The design and implementation will take place in publicly visible 
repositories and discussion forums. The process will use an RFC-style 
mechanism, with documents that pin down the design and with specified 
comment and conclusion timelines. 

The project leader will delegate RFC production and conclusion to 
groups of contributors that are identified by participation in the 
brainstorming phase (specification and implementation as well as 
discussion). Those groups will take feedback from the community at 
large, and they will be explicit about their rationales for final 
design decisions. Possible groups include a reader group, a macro 
group, a data structures and generics group, a library-organization 
group, and so on. 

CONCLUSION: When this phase produces sufficiently clear, detailed, and 
coherent specifications plus a significant implementation, the project 
can move to the next phase. 

Failure is still an option. If the project leader is never able to 
identify such a result, the project will be declared a failure. 


Phase 3: Conversion (months or years) 
------------------- 

GOAL AND OUTPUT: Complete language, libraries, and documentation, 
including a name for the language. 

PROCESS: This phase starts the attempt to port and adjust appropriate 
existing code and documentation (e.g., in the Racket main distribution) 
to make sure it works and to continue sorting out any mismatches 
between the new language and `#lang racket` at an even larger scale. 

A language name --- as opposed to a temporary project name --- must be 
picked at this point. By delaying the choice of name until we know what 
the language is, we avoid confusion and misinformation due to 
historical properties of Rhombus-in-development that end up not being 
true about the completed language. 

CONCLUSION: The decision of whether this conversion succeeds --- 
including which things really should be converted or not and when 
progress is sufficient to consider the next step --- is up to Racket 
project leadership. 

Failure is not yet ruled out. If the Racket project leadership never 
approves the language for direct support, then the project fails. 


Phase 4: Transition (years) 
------------------- 

GOAL AND OUTPUT: Unified distribution and presentation for the new 
language and the existing Racket ecosystem. 

PROCESS: If Rhombus is likely to appeal to a large number of people, 
the team will make adjustments to the existing Racket infrastructure: 

- the distribution, 

- the web pages, 

- the pedagogic material, 

- the communication channels, and 

- other outward-facing aspects. 

It's difficult to say what transition will be needed without knowing 
what the actual language will look like, but it's easy to predict that 
some transition will be needed if the Rhombus project manages to 
progress to this point. 

Racket project leadership, expanded with leaders emerging from the 
Rhombus project, will make the calls at this phase. 

Failure is no longer an option at this point. 

CONCLUSION: The new language is at least as well supported and 
available as `#lang racket`. 


What Will Happen to Racket by the End 
------------------------------------- 

Transitioning does not mean that Racket will disappear. 

- Existing `#lang racket` programs will continue to run beyond Phase 4. 

- The documentation for `#lang racket` will co-exist with whatever we 
call the new language. 

Put differently, Racket will become a component of the overall new 
distribution. 


- Jay, Matthew, Matthias, Robby, and Sam 
```
***
