This repository is the integration phase of the Rhombus project,
which is about the design of a new language that is based on Racket.

Rhombus offers the same kind of language extensibility as Racket
itself, but using conventional (infix) notation with the goal of
making Racket technology more accessible. For an overview of Rhombus's
goals and design, see

* [OOPSLA'23 paper](https://doi.org/10.1145/3622818)
  or the [talk video](https://www.youtube.com/watch?v=hkiy1rmKA48)

* [Documentation](https://docs.racket-lang.org/rhombus@rhombus/index.html)
  as rendered by the Racket package-build service

This repository also houses the packages that make up the current
Rhombus implementation.

# Trying Rhombus

Using [Racket](https://racket-lang.org), install the package `rhombus`
through DrRacket's "Install Package..." menu item or on the command
line with

```
raco pkg install rhombus
```

# Other Resources

* [Project plan](resources/plan.md)

* [General discussion](https://github.com/racket/rhombus-brainstorming/discussions)

* Older resources:
   - [State of Rhombus](resources/state-of-rhombus.md) October 2021
   - A summary of [possible goals](resources/goals.md)
   - References to some [mailing list discussion](resources/refs.md)
   - A summary of [potential syntax guidelines](resources/syntax-considerations.md)
   - An index of syntax [prior art](resources/prior-art.md)

# Participation

**Anyone can participate in Rhombus design discussions.** The Racket team's
unofficial motto is _anything we can do, you can do:_ programmers should feel
empowered to participate in the creation of the languages they use. Discussions,
pull requests, and issues are open to all, and a wide variety of perspectives is
especially beneficial.

## Issues and Pull Requests

GitHub [**issues**](../../issues) are for bug reports and feature
ideas, and [**pull requests**](../../pulls) are for bug-fix candidates
and other concrete proposals.

## Conversations as Discussion

GitHub [**discussions**](../../discussions) provide a forum for discussing
Rhombus broadly, as opposed to discusssing details of specific
proposals.
