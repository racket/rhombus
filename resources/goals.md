# Motivation
[motivation]: #motivation

This document is intended to distill the main objectives of the project as explained in various threads and provide a resource to help orient discussion. It will evolve with the discussion.

Issues and pull requests are expected to refine and elaborate points brought up in this document.  Similar to [syntax considerations](syntax-considerations.md), this should be considered a living document useful mostly for flushing out questions and reactions.

This document, or the concepts in it, could also be used to help newcomers quickly orient themselves to some goals that may not have been apparent in the initial e-mails.  For a more detailed discussion with more nuances, it's still useful to read the full threads in [prior art](prior-art.md) (among others).

# Goals List
[goals-list]: #goals-list

* more generics
* more consistency
* lower barriers of entry
* extend smooth, fine-grained language extensibility to a broader set of syntaxes
* make backwards-incompatible changes that reflect newer thinking (e.g., structs)

# Unresolved questions
[unresolved-questions]: #unresolved-questions

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
