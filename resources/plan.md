# Rhombus Plan

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
the 6.x, 7.x, and 8.x series of releases. 

- The team will release updated versions, occasionally making modest 
incompatibilities with explicit transition paths as needed—all 
as usual. 

This does not mean that the language and its implementation will evolve 
at the same speed as it has in the past, but it means that we will 
maintain our standard commitment to reliability and quality. 


Phase 1: Brainstorming (months) 
---------------------- 

GOAL AND OUTPUT: A design sketch and collection of prototype 
implementations that reflect key ideas and design constraints. 

PROCESS: This phase is a discussion of ideas and  potential directions at
[https://github.com/racket/rhombus-brainstorming](https://github.com/racket/rhombus-brainstorming).
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

The design and implementation will take place at
[https://github.com/racket/rhombus-prototype](https://github.com/racket/rhombus-prototype)
(which is the same repo as brainstorming, but renamed for this phase).
The process will use an RFC-style mechanism, with documents that pin
down the design and with specified comment and conclusion timelines.

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


Phase 3: Integration (months or years) 
------------------- 

GOAL AND OUTPUT: Complete language, libraries, and documentation, 
including a name for the language. 

PROCESS: This phase starts the attempt to port and adjust appropriate 
existing code and documentation (e.g., in the Racket main distribution) 
to make sure it works and to continue sorting out any mismatches 
between the new language and `#lang racket` at an even larger scale. 

A language name—as opposed to a temporary project name—must be 
picked at this point. By delaying the choice of name until we know what 
the language is, we avoid confusion and misinformation due to 
historical properties of Rhombus-in-development that end up not being 
true about the completed language.  [Note: In the end, we picked
Rhombus for the language name, too.]

CONCLUSION: The decision of whether this conversion succeeds—
including which things really should be converted or not and when 
progress is sufficient to consider the next step—is up to Racket 
project leadership. 

Failure is not yet ruled out. If the Racket project leadership never 
approves the language for direct support, then the project fails. 


Phase 4: Production (years)
------------------- 

GOAL AND OUTPUT: Unified distribution and presentation for the new 
language, possibly prominently branded in the Racket ecosystem.

PROCESS: If Rhombus is likely to appeal to a large number of people, 
the team will make adjustments to the existing Racket infrastructure: 

- the distribution, 

- the web pages, 

- the pedagogic material, 

- the communication channels, and 

- other outward-facing aspects. 

It's difficult to say what adjustments will be needed without knowing
what the actual language will look like, but it's easy to predict that
some new and unifying material will be needed if the Rhombus project
manages to progress to this point.

Racket project leadership, expanded with leaders emerging from the 
Rhombus project, will make the calls at this phase. 

Failure is no longer an option at this point for the new language,
but its prominence within Racket will depend on the community.

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


# Edit history

Originally [posted on the Racket mailing list](https://groups.google.com/d/msg/racket-users/-x_M5wIhtWk/V47eL30HCgAJ)
from Racket project leadership on 2019/10/02.
