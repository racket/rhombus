This repository is for brainstorming and discussion for the Rhombus
project, which is about the design of a new language that is based on
Racket. “Rhombus” is the project name and a placeholder name for
a language name to be chosen later.

# Resources

* [State of Rhombus](https://docs.google.com/document/d/10GTdmxo6Uty_-SQY8hrz5unCwtNi_YIsuI5yghmZ6hU)

* Active proposals:
    - [Shrubbery notation](https://github.com/mflatt/rhombus-brainstorming/blob/shrubbery/shrubbery/0000-shrubbery.md)
      [(discussion)](https://github.com/racket/rhombus-brainstorming/pull/122)
    - [Enforestation](https://github.com/mflatt/rhombus-brainstorming/blob/enforestation/enforestation/0000-enforestation.md)
      [(discussion)](https://github.com/racket/rhombus-brainstorming/pull/162)
    - [Rhombus protoype](https://github.com/mflatt/rhombus-brainstorming/blob/rhombus/rhombus/0000-rhombus.md)
      [(discussion)](https://github.com/racket/rhombus-brainstorming/pull/163)

* [General discussion](https://github.com/racket/rhombus-brainstorming/discussions)

* Older resources:
   - A summary of [possible goals](resources/goals.md)
   - References to some [mailing list discussion](resources/refs.md)
   - A summary of [potential syntax guidelines](resources/syntax-considerations.md)
   - An index of syntax [prior art](resources/prior-art.md)

# Participation

**Anyone can participate in Rhombus design discussions.** The Racket team's
unofficial motto is _anything we can do, you can do:_ programmers should feel
empowered to participate in the creation of the languages they use. Discussions
in pull requests and issues are open to all, and a wide variety of perspectives
is especially beneficial.

# Using this GitHub Repository

## Conversations as Discussion

GitHub [**discussions**](discussions) provide a forum for discussing
Rhombus broadly, as opposed to discusssing details of specific
proposals.


## Issues as a Wish List

GitHub [**issues**](issues) for this repository represent a kind of wish list.
Post there for an idea about some way that you think a new language
should be different than the current Racket language. Discuss other
suggestions there.

## Pull Requests as Draft Proposals

GitHub [**pull requests**](pulls) for this repository can represent more concrete
proposals. Proposals here do not have to be exhaustive, but they
should be concrete enough to enable discussion of the pros and cons of
a specific solution. There can and should be multiple conflicting
proposals for any particular problem or direction, especially at
first. The intent is to enable brainstorming and discussion toward a
future, more-complete proposal.

To make a draft proposal, start with "template.md" and create either a
new ".md" file or a subdirectory containing a ".md" file and other
supporting material (such as an implementation).

## Pull Requests as Resource Updates

GitHub [**pull requests**](pulls) for this repository can also be suggestions
to update resources listed above. These resources are intended to be
uncontroversial summaries, although they may point toward
controversial or conflicting perspectives.

For these kinds of pull requests, please include "[resource]" at the
start of the pull request title.
