- Feature Name: Principles for inclusion of new `#lang`s in core
- Start Date: 2019-08-11
- RFC PR: [racket/racket2-rfcs#0002](https://github.com/racket/racket2-rfcs/pull/104)

# Summary
[summary]: #summary

This is a meta RFC for determining the process by which RFCs about new `#lang`s and
new syntax will be considered for inclusion in the Racket core implementation as
part of a community process.

# Motivation
[motivation]: #motivation

The Racket community is about to embark on an amazing new journey into
Language Oriented Programming. If we succeed there will be a little
language to solve every problem. Thus, as a community we are faced with
a dilemma: which of those little langs should be allowed to be used as
part of the core implementation of the communal language? The easy case
is "All of them!" unfortunately a quick look over some examples will
reveal that that is essentially impossible. Thus we need to determine
the criteria that will be used to evaluate, and approve <span
class="underline">ANY</span> language that might be used in the core
implementation.

The payoff for developing this process as a community is that it will
provide a solid future process that will be open to anyone.  If we can
agree on the process, we have a much better chance of accepting the
outcomes.

This RFC lays out a set of key questions that need to be answered by the
core team and community that bear directly on the relevance of this RFC.

It then proposes a set of rules for RFCs about syntax, followed by a
general set of rules for RFCs about inclusion of any new `#lang` in the
core implementation.

It then lists a some potential dimensions for evaluating any language
for inclusion in the core implementation. These criteria will cover the
following.

-   runtime
-   syntax
-   implementation
-   test coverage
-   dependencies
-   documentation
-   tooling

Ultimately both the dimensions and the minimal criteria a language must
meet should be determined by the community as part of the overall RFCing
process.

## A motivating query

Current recommendations for writing libraries and contributing to the
core implementation of Racket are to use `racket/base`.

To motivate the imagination about some future "change" or shall we say
"liberalization" (perhaps "reformation"?) of the core Racket language,
here are some contemporary examples for consideration.

What if I wanted to implement part of the Racket core in `#lang scheme`?
`#lang bsl`? `#lang typed/racket`? `#lang hackett`? How about
`#lang rackjure`? Maybe `#lang bf`? Better yet `#lang rosette`? How
about [`#lang c`](https://github.com/jeapostrophe/c/blob/master/c/lang/runtime.rkt#L10).

All of these are technically possible, the question then is which of
them if any would be sound technical decisions and what criteria would
we use to determine that?

I suggest that whatever more complete criteria are developed from the
dimensions below and elsewhere, that we uses these languages (or a
similar set) to test how those criteria perform in general.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

## RFCs about syntax.

Since this RFC implicitly assumes that the RFCing process will center
around actual `#lang` implementations, an additional suggestion for
rules governing syntax related RFCs is put forward here for
consideration.

1.  All RFCs that deal with systematic syntactical changes or additions
    will only be considered if they are accompanied
    by a full working implementation as a `#lang`, along with certain
    tooling and documentation as yet to be determined by the community as
    part of this RFCing process.
2.  RFCs related to syntax that do not have a working `#lang`
    implementation and accompanying tooling and documentation
    are not be eligible for any official status.
3.  Such RFCs may still be submitted, but in the absence of
    implementation, tooling, and documentation, they will be
    considered ineligible for further consideration until such a time as the
    implementation, tooling, and documentation are available to the
    community.

## Proposed criteria for RFCs about `#lang`s

All RFCs that deal with or propose the use of a new `#lang` for use in
the core implementation shall provide for consideration the following.

1.  A working implementation as a `#lang`
2.  Documentation of any and all new forms in the syntax proposed.
3.  The following tooling
    1.  syntax highlighting
    2.  pretty printer
    3.  converters to and from s-exp syntax (where relevant)
4.  The results of measures on the dimensions of the `#lang` that are
    determined by the community to be relevant for assessing a language
    for inclusion. Some potential dimensions are listed in the next
    section.

All RFCs are suggested to provide the following.

1.  The use cases that the language and syntax are optimized for or were
    designed for as well as their indented audience.
2.  An initial consideration of tradeoffs/costs/benefits of any new
    syntax from the perspective of the proposing member(s).

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## Dimensions

A number of these dimensions are listed as a matter of completeness. In
practice some have obvious, or trivial requirements, such as the Racket
version that they support, which would clearly have to be at least the
current version. Another example is the required runtime, where it seems
fairly uncontroversial to add a restriction that the `#lang` should
support `racketcs` and must not depend on any external runtime or
foreign functions. Certain `#lang` s might not need to address as many
of the points, for example if a `#lang` uses s-expression syntax then it
would be sufficient to state that that is in line with current communal
standards as well as in line with the current core implementation.

### Runtime

1.  Which Racket runtimes does the `#lang` support?

    -   racketcs
    -   racket3m
    -   racketcgc
    -   pycket

2.  What versions of racket are supported?

    This is included as a trivial case where a `#lang` that only works in
    old versions of Racket would be ruled out.

3.  Does the `#lang` depend on a runtime beyond the core Racket runtime?

4.  Does the `#lang` use the FFI?

### Syntax

This is an incomplete list of high level properties many of which could
be operationalized to evaluate `#lang` s for use in the core. This list
partially overlaps the list in the
[syntax-considerations](https://github.com/racket/racket2-rfcs/blob/master/syntax-considerations.md#considerations-list)
document.

1.  Homogeneity

    Local structure, such as homogeneity, regularity etc. Is there
    always local correctness of the surface syntax?

2.  Regularity of visual form

    This is a tough one – the primary issues is that in Racket you can
    use \[\] and {} as a way to visually group blocks to make them stand
    out and easier for people who read code visually (rather than
    verbally / mathematically) to literally see what is going on.

3.  Confusability (possibly fits under robustness)

    How easy is it to change a single visually similar character and
    get an unexpected, nasty result? If the result is silent failure, a
    'correct' program with a different meaning then the syntax is very
    broken. If you want an example of such a language, look no further
    than Python.

4.  Robustness / fail loudly

    1.  The robustness of local syntax to incorrect/errors/erroneous in
        code that is 'nearby'

        For example, aside from mispairing of paired forms ("", (), {},
        \[\], etc.) lisp is extremely robust. This is largely due to the
        homogeneity of s-expression surface syntax.

    2.  The difficulty of making syntactic errors

        As an example, in a variety of languages, it is easy to forget
        the semi colon that ends a statement and the computer can't help
        you (unless javascript's handling of semi colons strikes one as
        a good idea). Compare this to parens. The computer never misses
        a paren unless the pbkac operating it goes out of their way to
        defeat the default behavior, maybe c editors should terminate
        every line with a semi colon by default and force the users to
        remove them if they don't need them?

        Commas are another example.

        Commas are noise, they are more keystrokes, they are easy to
        forget, and having forgotten them many times in python, let me
        tell you, you can forget a comma and have a syntactically
        correct program that does something completely other than what
        you expected – clojure gets this right, commas should be treated
        as whitespace, if someone wants them for cosmetic or purely
        spatial reasons, then by all means let them.

        Interestingly, parens are another example here as well. In the
        Racket let form, while annoying for someone who is only binding
        a single value `(let ([why so-many-parens?]) body)` as soon as
        they move on to more advanced usage they often discover that the
        parens actually make it easier to read the resulting code rather
        than a clojure like let form where everything runs in pairs
        `(let (oh no i am lost where do these pairs end!?) foo)` vs
        `(let ([oh no] [i am] [lost where] [do these] [pairs end!?]) foo)`
        sometimes visual markers provide regular structure which spaces
        alone do not. It is hard to improve on having a single pair of
        symbols that allow for the visual differentiation of sections of
        code.

    3.  How easy is it to make silent mistakes?

        The cost of forgetting 1 thing vs the difficulty/distance
        between correct/incorrect of missing 1 thing. Essentially is
        there a way we can prove that the search space for a missing
        semicolon is vastly larger than the search space for a missing
        paren? Yes, because my surface syntax with only rainbow
        delimiters and unmatched paren counting is all I need to find
        the first missing paren, and it gives incredibly good locality.

5.  Complexity of required parser

    There are many levels for this, one would be a full parse, another
    would be syntax highlighting, yet another would be paredit, another
    would be structural editing, etc.

6.  Difficulty of implementation independent of the underlying runtime

    In this case we ignore the runtime complexity since we assume it is
    all Chez Scheme the question here is how hard it is to implement
    basic parsing, perhaps with some semantics. For example, I have
    implemented a couple of parsers that work for basic Racket in other
    languages so that I can share data portions of my codebases between
    languages. Enforestation seems like it could significantly
    complicate matters this is also relevant for promoting adoption in
    communities that use other editors where good tooling would become
    more difficult to implement

7.  Number of keystrokes required

    Aside from basic usability, there are a variety of injuries and
    disabilities that are made worse by having to type more. This
    factors into the original authoring of code, as well as refactoring.
    The refactoring is related to tooling though, so original authoring
    is easier to evaluate.

8.  The size of the EBNF

    when controlling for the complexity of the runtime (`#lang` bf has
    different semantics so a smaller EBNF is expected)

9.  The number of characters with meaning reserved by the syntax

    Related to EBNF size.

10. Terseness

    This is likely conflated with simplicity and expressiveness and is
    likely inversely related to the number of characters with reserved
    syntactic meaning.

11. Toolability

    See the section on tooling, dealing with the actual existence
    implemented tooling.

    1.  ability of tools to reduce number of required keystrokes

        We don't write computer programs with pen an pencil anymore, and
        computers are REALLY good at pairing pares, they even do it in
        languages where the parens don't really matter.

    2.  ability to autoformat (pretty print) in a way that is
        deterministic

        This might be one of the implementation requirements for any new
        syntax. It doesn't have to be a <span
        class="underline">good</span> deterministic formatting, but it
        needs to be possible and it needs to exist.

12. Total possible complexity of an identifier

    Why can't I use `-` in my identifier?

13. The amount of implicit information that cannot be inferred directly
    from the syntax

    For example implicit operator precedence. If there are things that
    you have to go into the ground floor implementation to look at, or
    worse, the documentation, then that is bad. All forms require some
    definition at some point, however having special cases that must be
    defined in the reader for the language to parse correctly is bad.

14. Popularity

    How popular are languages with 'similar' syntax independent of the
    popularity of their runtime? One might also need to account for age
    and total corporate backing here. There are countless factors that
    have nothing to do with the actual quality of syntax that
    nonetheless are vital for popularity. This is provided as a
    potential 'trap' dimension since there is very little reason to
    believe that popularity of one syntactically similar language does
    much if anything to bless those around it with similar syntax.

15. Teachability, familiarity

    Noting that adolescent males might be a bad control group for
    assessing this. Maybe adolescent male homo sapiens are actually a
    bunch of curmudgeons that don't want to learn things they don't have
    to if they can convince the teacher that they shouldn't? And maybe
    only the CS majors?

16. The amount of time it takes to explain

17. The number of lines that it takes to define

    The basic syntax, the number of elements etc, related to EBNF.

18. Closeness to previously existing syntaxes

    This addresses the desire for a distinct, recognizable syntax. Note,
    most novices (my past self included) cannot tell the difference
    between Common Lisp and Racket by syntax alone. They use semantic
    cues, such as define vs defun/defvar, defmacro vs define-syntax etc.
    with the possible exception of () vs \[\] and {}.

19. Extensibility

    What are the dangers of undelimited syntactic extensibility?
    What challenges would be imposed by a lisp that allowed any
    position in an s-exp to be the form that triggers a macro?

21. Expressiveness

    On the Expressive Power of Programming Languages
    <https://doi.org/10.1016/0167-6423%2891%2990036-W>

22. Compatibility

    Can forms be share directly with other languages? Is there a way to
    invoke a function defined in `lang x` in the core language? This may
    not apply to all languages where their scope is intentionally
    delimited to simply make it easier to express a particular structure
    rather than provide general functionality.

    **NOTE** This is orthogonal to the question about the impact on the
    community of being able to use a form vs being able to debug it and
    understand/contribute to/modify its implementation.

    1.  Require the `#lang`s forms in other `#lang`s

        and use them with the standard syntax and conventions of the
        consuming `#lang`

    2.  require other `#lang`s forms in the `#lang`

        and use them in the syntax of the current `#lang`

### Implementation

1.  Is there a working implementation as a `#lang` ?

2.  Maintainability.
    This could fit in a number of different categories,
    but the basic question is whether there is someone
    who is capable and willing to maintain the implementation
    There is a related question about whether the code written
    in the `#lang` is maintainable, which might fit under the
    syntax/general properties listed above.

### Test coverage

1.  Are there tests?

2.  If there are converters, are there tests for them?

### Dependencies

All of these questions apply to transitive dependencies in addition to
direct dependencies.

1.  Does the `#lang` depend on a library written in a non-approved for
    core use `#lang`?

2.  Would the inclusion of the `#lang` induce circular dependencies?

    I'm not sure the extent to which this would be an technical issue vs
    a practical maintenance issue, since one can usually do a little
    require/provide dance to get things loading in the right order.
    Affordances around which `#lang` s can be used at what point in the
    core bootstrapping hierarchy is probably related to the question of
    composability as well.

### Documentation

1.  Is there scribble documentation?

2.  What percentage of the forms in the lang have documentation written
    in scribble.

3.  What percentage of the forms in the lang have usage examples in the
    documentation?

### Tooling

Do these tools exist?

1.  syntax highlighting

    -   drracket
    -   emacs
    -   vim
    -   atom
    -   vscode
    -   etc …

2.  structured or semi-structured editing

3.  pretty printer

4.  converter from `#lang racket/base`

    Does the converter have inverse problem? i.e., grammars written in
    `#lang brag` trivially have a converter into `#lang racket/base`

5.  converter to `#lang racket`

6.  docs converter

    Unfortunately a `#lang` that does not have scribble support still
    must be evaluated on the fact that it will not have fully featured
    documentation.

# Drawbacks
[drawbacks]: #drawbacks

It will take time to determine the process by which criteria for
evaluation will be selected. A governance model and tracking/determination
of what official status an RFC can attain will have to be implemented.

It will take time to develop and implement the set of objective criteria for
evaluating new languages for inclusion in the core.

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

For rationale see Motivation.

One alternative to this process are that syntax related proposals are
evaluated in the absence of some objective criteria and instead by
vote, by discussion, or by fiat.

# Prior art
[prior-art]: #prior-art

The [syntax-considerations](https://github.com/racket/racket2-rfcs/blob/master/syntax-considerations.md)
doc is extremely relevant to this RFC. This RFC could be considered to
directly address future possibility for defining categories of considerations.

Potential prior art might be the process, if any, that the linux
kernel uses for including lex/yacc based languages in the codebase.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

- Is this RFC relevant?
- What further RFCs are needed to develop the process and determine the criteria for inclusion?
- How much work will it be to develop operational definitions for some of the dimensions vs
  a semi-quantitative or qualitative assessment?

# Future possibilities
[future-possibilities]: #future-possibilities

Next steps are for the community to determine what dimensions are
relevant for evaluating a language and which criteria for evaluating
them should be mandatory, quantative/multidmensional, and optional.
