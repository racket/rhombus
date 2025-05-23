#lang rhombus/scribble/manual
@(import: "quote.rhm" open)

@title{Rationale}

The token-level syntax is chosen to be familiar to programmers
generally. The sequence @litchar{1+2} is one plus two, not a strangely
spelled identifier. Tokens like @litchar{(}, @litchar{,}, @litchar("{")
and @litchar{;} are used in familiar ways. Shrubbery notation provides
enough grouping structure that code navigation and transformation should
be useful and straightforward in an editor.

Parentheses in shrubbery notation do not disable indentation, unlike
some indentation-sensitive notations. That choice supports a language in
shrubbery notation where parentheses can be added around any expression
--- even if the expression is written with indentation (although the
expression may need to be shifted right to preserve relative
indentation, depending on how parentheses are added).

The inclusion of @litchar{'} as a parenthesis-like form reflects how
shrubbery notation is intended as a vehicle for metaprogramming contexts
where quoting program terms is common. Using the same character as the
opener and closer creates some hassle for certain nesting cases, but
those are relatively rare. Meanwhile, the use of @litchar{'} to mean a
form of quoting should be familiar to programmers, although there is a
risk that @litchar{'}s will be misunderstood as string quoting.

The inclusion of @litchar{|} in shrubbery notation reflects the fact
that conditional forms (such a @litchar{if}, @litchar{cond}, and
@litchar{match}) are important and common. A distinct, pleasant, and
uniform pattern for conditionals deserves direct support in the
notation.

Requiring a preceding @litchar{:} or preceding/following @litchar{|} for
block-creating indentation is mostly a kind of consistency check to
enable better and earlier errors when indentation goes wrong. It also
allows indentation that starts with an operator to continue a group;
it's possible for bad indentation to inadvertently cause an operator to
be treated as continuing a group, but hopefully that will be rare.
Always requiring a preceding @litchar{:} before an indented @litchar{|}
line would be consistent, but it adds extras @litchar{:}s where
@litchar{|} already provides one consistency check.

A @litchar{:} block or @litchar{|} alternatives must appear at the end
of a group, because @litchar{:} and @litchar{|} lack a specific closing
character. Forms that expect both a @litchar{:} block and @litchar{|}
alternatives in a group seem unlikely to be common, but allowing both
supports certain syntactic forms (e.g., an operator that is defined
through multiple pattern-matching cases, but with a block before all
cases to specify precedence and associativity). Allowing the
combination, in turn, naturally leads to allowing an empty-block
@litchar{:} before a sequence of @litchar{|} alternatives, effectively
making a @litchar{:} before @litchar{|} optional. Normalizing to drop
the empty block in that case, instead of preserving it before the
sequence of alternatives, makes shrubbery notation feel more consistent
without burdening consumers of shrubbery forms to explicitly accommodate
an empty block.

Explicit block grouping via @litchar{«} and @litchar{»} is expected to
be rare. The grouping characters were intentionally chosen from the
Latin-1 extension of ASCII to avoid reserving additional ASCII
characters.

Making whitespace and comment lines ignored in all contexts means that
they can be freely added without interfering with grouping. The
@litchar{\} continuation operator is somewhat unusual in that it skips
blank and comment lines to continue, as opposed to requiring @litchar{\}
on every continuing line; that, too, allows extra blank and comment
lines to be added, even amid continuing lines, although at the risk that a
@litchar{\}'s reach will extend further than intended past blank
lines.

Using @litchar{~} for keywords has a precedent in OCaml. Reserving
@litchar{~} for keywords exclusively would use up a character that
might otherwise be used for operators, and so @litchar{~} is also
allowed as in operator as long as it is combined with other operator
characters. The notion of keywords as distinct from identifiers has
been liberating for Racket syntax (particularly since keywords can be
kept disintinct from expressions more generally), and we expect
similar benefits for having keywords in shrubbery notation.

The @s_exp_braces escape to S-expressions provides a bridge between
shrubbery notation and Racket identifiers. For example,
@litchar{#{exact-integer?}} is an identifier with @litchar{-} and
@litchar{?} as part of the identifier. Shrubbery notation could be
adapted to support Lisp-style identifiers by requiring more space around
operators, but the rule for continuing a group between @litchar{(} and
@litchar{)}, @litchar{[} and @litchar{]}, or @litchar("{") and
@litchar("}") currently depends on distinguishing operators from
non-operators.

For @litchar("@"), the choice of treating @litchar|{@f(arg){text}}| as
@litchar{f(arg, ["text"])} instead of @litchar{f(arg, "text")} reflects
experience with S-expression @litchar("@") notation. Although it seems
convenient that, say, @litchar|{@bold{x}}| is treated as @litchar{(bold "x")},
the consequence is that a function like @litchar{bold} might be
implemented at first to take a single argument; later, a use like
@litchar|{@bold{Hello @name}}| breaks, because two arguments are
provided. Making explicit the list that's inherent in body parsing
should help reduce such mistakes (or bad design choices) for functions
that are meant to be used with @litchar("@") notation.
