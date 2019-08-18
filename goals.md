# Summary
[summary]: #summary

An informal summary of some of the vectors of improvement to be explored by Racket2.  This is meant to complement the [syntax considerations](syntax-considerations.md), serving a similar but broader role.


# Motivation
[motivation]: #motivation

While still relatively early, there has already been ample discussion.  Due to the volume of discussion and its rapid evolution, it may be difficult to extract some of the key highlights as they stand now.  In particular, some of the reasons for the new syntax experiment may be hard to identify without careful reading of the whole threads in the [README](README.md) and other mailing list posts.

This document is intended to distill the main objectives of the project as explained in those threads and provide a resource to help orient discussion.


# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

At this stage, we should consider these goals preliminary.  Issues or RFCs are expected to come out of the points brought up in this document.  Similar to syntax considerations, this should be considered a living document useful mostly for flushing out questions and reactions.

This document, or the concepts in it, could also be used to help newcomers quickly orient themselves to some goals that may not have been apparent in the initial e-mails.  For a more detailed discussion with more nuances, it's still useful to read the full threads in [Prior Art](#prior-art) (among others).


# Goals List
[goals-list]: #goals-list

* more generics
* more consistency
* lower barriers of entry
* extend smooth, fine-grained language extensibility to a broader set of syntaxes
* make backwards-incompatible changes that reflect newer thinking (e.g., structs)


# Prior art
[prior-art]: #prior-art

* [Racket2 and syntax](https://groups.google.com/d/msg/racket-users/3aIPOGbGgmc/A4HHSbdxAwAJ)
* [Racket2 possibilities](https://groups.google.com/d/msg/racket-users/HiC7z3A5O-k/XPR2wbSJCQAJ)
* [Clarify project policy on Racket2 syntax](https://groups.google.com/d/msg/racket-users/9Eh9H1Jt28Q/JXyDBfFrAgAJ)


# Unresolved questions
[unresolved-questions]: #unresolved-questions

* Is there a name for this document that better describes what it is?
* What is the framework for bringing this from abstract goals to concrete steps?  What about even more abstract/higher level goals?
* Should there be "sacred cows" or constraints in this document?
* What does "lower barriers" mean?  What does "programmers generally" mean, given how heterogeneous that group is?  HOW heterogenous is the group?  (What would lower barriers *for whom*?)
* What are specific obstacles teachers have had?
* How much do we want to reach out to "non-programmer" programmers?
* Are there particular concepts we want to encourage/discourage?  Lessons we want programmers to learn?

Potentially relevant but outside the scope of this document:
* How much community support is available for tooling, etc.?
* How extensive will the interoperability between Racket2 and #lang racket be? (This is not just about syntax!)
* What will be the relationship between Racket2 and the existing #lang racket?


# Future possibilities
[future-possibilities]: #future-possibilities

* Add a FAQ section (or document)
* Modify (or replace) this document to reflect decisions or constraints
* Develop into either a "mission statement" or even messaging/marketing guidance
