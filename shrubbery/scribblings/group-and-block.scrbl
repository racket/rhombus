#lang scribble/rhombus/manual
@(import: "grammar.rhm" open)

@(def opener: @emph{opener})
@(def closer: @emph{closer})
@(def opener_closer: @elem{@opener--@italic{closer}})

@title[~tag: "group-and-block"]{Groups and Blocks}

The heart of shrubbery notation is its set of rules for organizing
@deftech{terms} into @deftech{groups}, @deftech{blocks}, and
@deftech{alt-blocks}. The following grammar summarizes the abstract
structure of a shrubbery-notation document, where literal fragments like
@litchar{(} serve merely as tags:

@nested[~style: symbol(inset),
        bnf.BNF([@nonterm{document},
                 kleenestar(@nonterm{group})],
                [@nonterm{group},
                 bseq(kleenestar(@nonterm{term}), @nonterm{tail-term})],
                [@nonterm{term},
                 @nonterm{atom},
                 balt(bseq(@litchar{(}, kleenestar(@nonterm{group}), @litchar{)}),
                      bseq(@litchar{[}, kleenestar(@nonterm{group}), @litchar{]}),
                      bseq(@litchar["{"], kleenestar(@nonterm{group}), @litchar["}"]),
                      bseq(@litchar["'"], kleenestar(@nonterm{group}), @litchar["'"]))],
                [@nonterm{tail-term},
                 @nonterm{term},
                 @nonterm{block},
                 @nonterm{alt-block}],
                [@nonterm{block},
                 bseq(@litchar{:}, kleenestar(@nonterm{group}))],
                [@nonterm{alt-block},
                 bseq(@litchar["|"], kleenestar(@nonterm{block}))])]

A document is a sequence of groups, each of which is a non-empty
sequence of terms. Terms include @deftech{atoms}, which are either
individual @seclink["lexeme-parsing"]{lexeme tokens}, @opener_closer
pairs that contain groups, or blocks as created with @litchar{:} or
@litchar{|}---but a block as a term is constrained to appear only at the
end of a group.

An alt-block, as created with @litchar{|}, is a special kind of block
that contains a sequence of blocks. Conceptually, each nested block in
an alt-block is in a group, but the group is constrained to have a
single block, so a layer is collapsed in the abstract structure.
The fact that an alt-block was created with @litchar{|} is tagged in the
abstract structure, much in the same way that specific @opener_closer
pairs @litchar{()}, @litchar{[]}, @litchar{{}}, and @litchar{''} tag
the terms that they create.

Although an @opener_closer pair contains a sequence of groups, that
content is not a block, since it is not created via @litchar{:} or
@litchar{|}. Similarly, a document overall is a sequence of groups but
not a block.

@section{Grouping by Lines}

The main grouping rule is that sequences on different lines with the
same indentation create separate @tech{groups}, one for each line.

@(rhombusblock:
    this is the first group
    this is the second group
  )

Comments and lines with only whitespace are ignored. They don't count
when this document says “the previous line” or “the next line.”

@section{Grouping by Opener--Closer Pairs}

An @opener_closer pair @litchar{(} and @litchar{)}, @litchar{[} and
@litchar{]}, @litchar["{"] and @litchar["}"], or @litchar{'} and
@litchar{'} (those two are the same character) forms a @tech{term} that
can span lines and encloses nested groups. Within most @opener_closer
pairs, @litchar{,} separates groups, but @litchar{;} separates group
with a @litchar{'} pair. Groups can be on separate lines at the same
indentation, but groups on separate lines still must be separated by
@litchar{,} in @litchar{()}, @litchar{[]}, or @litchar{{}}. Parsing
retains whether a subgroup is formed by @litchar{()}, @litchar{[]},
@litchar{{}}, or @litchar{''}.

@(rhombusblock:
    group 1
    [group 2 - subgroup I,
     group 2 - subgroup II,
     (group 2 - subgroup III - subsubgroup A,
      group 2 - subgroup III - subsubgroup B,
      {group 2 - subgroup III - subsubgroup C, subsubsubgroup α,
       group 2 - subgroup III - subsubgroup C, subsubsubgroup β})]
    'group 3 - subgroup I;  group 3 - subgroup II
     group 3 - subgroup III'
  )

The following three forms are not allowed, because they are missing a
@litchar{,} between two groups:

@verbatim[~indent: 2]{
// Not allowed
(1
 2)
[1
 2]
{1
 2}
}

A @litchar{,} is disallowed if it would create an empty group, except
that a trailing @litchar{,} is allowed.

@verbatim[~indent: 2]{
// Not allowed
(, 1)
(1,, 2)

// Allowed, but not standard
(1, 2,)
}

A trailing @litchar{,} is only standard style when the @closer that follows is
on its own line.

@(rhombusblock:
    list(
      red,
      green,
      blue,
      orange,
    )
)

Using @litchar{'} as both an @opener and @closer prevents simple nesting
of those forms. There is no problem if a @litchar{(}, @litchar{[}, or
@litchar["{"], appears between one @litchar{'} as an opener and another
@litchar{'} as an opener; otherwise, two consecutive @litchar{'}s
intended as openers would instead be parsed as an opener and a closer.
To disambiguate, @litchar{«} can be used immediately after immediately
after an opener @litchar{'}, and then @litchar{»} must be used just before the
closing @litchar{'}. The @litchar{«} and @litchar{»} are @emph{not}
preserved in the parsed representation.

@(rhombusblock:
   'a ('nested') b'
   '«a 'nested' b»'
)

@section{Blocking with @litchar{:} and Indentation}

A sequence of groups has a particular indentation that is determined by
the first group in the sequence. Subsequent groups in a sequence must
start with the same indentation as the first group.

@verbatim[~indent: 2]{
group 1
group 2
// error, because the group is indented incorrectly:
  group 3
}

When a line ends with @litchar{:} and the next line is more indented, then
it starts a new sequence of groups that form a @tech{block}:

@(rhombusblock:
    group:
      subgroup 1
      subgroup 2
)

There is no constraint on how much indentation a nested group sequence
must use, as long as the indentation is more than the enclosing group.
Also, a new line is not required after @litchar{:}, but then it's as if the
@litchar{:} is followed by a newline plus spaces that reach the same column as
the @litchar{:}. All four of the following groups are the same, each with one
block that has two nested groups:

@(rhombusblock:
    hello:
     world
     universe

    hello:
           world
           universe

    hello: world
           universe

    hello:    world
              universe
  )

Within an @opener_closer pair, a nested group sequence can start at
any indentation; it doesn't have to be indented to the right of the
@opener.

@(rhombusblock:
    function(
      argument,
      more
    )
  )

A block that is started with @litchar{:} normally cannot be empty
(unless explicit-grouping @litchar{«} and @litchar{»} are used as
described in @secref["guillemot"]), so the following is ill-formed:

@verbatim[~indent: 2]{
bad_empty:  // empty block disallowed
}

However, @litchar{:} can be used at the start of a group so that the group
contains only a block. When @litchar{:} starts a group that is in the
top-level sequence or within an @opener_closer pair, the block
created by @litchar{:} is allowed to be empty (because that provides a way to
express an empty block in a context where it likely to be intentional
instead of confusing). For example, the first of the following three
top-level groups has just a block that contains one group with the
single element @litchar{untagged}, the second top-level group has just a
block with zero groups, and the third has a group with one parenthesized
sequence of groups where the middle one has an empty block:

@verbatim[~indent: 2]{
    : untagged

    :

    (1, :, 2)
}

@section[~tag: "continuing-op"]{Continuing with Indentation and an Operator}

When a newly indented line starts with an operator and when the
preceding line does @emph{not} end with @litchar{:}, then the indented line
does not form a block, and it may instead continue the previous line.
The operator-starting line continues only if the previous line was not a
continuing line; however, additional continuing lines can start with an
operator (not necessarily the same one) at the same indentation as the
original continuing line. The following two groups are the same:

@(rhombusblock:
    f(1) + 2
      + 3 + 4
      - 5 - 6

    f(1) + 2 + 3 + 4 - 5 - 6
  )

A block is always at the end of its immediately containing group. One
consequence is that an operator-starting line cannot continue a group
that already has a block:

@verbatim[~indent: 2]{
hello: world
  + 3 // bad indentation
}

Along those lines, there is no ambiguity when an indented line appears
after @litchar{:} and starts with an operator. In that case, the indented line
is part of the block, since it cannot continue the group that contains
the block. For example, the following two groups are the same, each
with a block that has a @litchar{+ 3} group:

@(rhombusblock:
    hello: + 3

    hello:
      + 3
  )

@section[~tag: "alt-block"]{Blocking with @litchar{|}}

A @litchar{|} is implicitly shifted half a column right (so, implicitly
nested), and it is implicitly followed by a @litchar{:} that
conceptually occupies same column as the @litchar{|}. That is, like
@litchar{:}, a @litchar{|} always creates a nested block. Furthermore,
@litchar{|} starts an enclosing block that includes the @litchar{|}
block plus subsequent @litchar{|} blocks that are at the same
indentation. An overall sequence of blocks created with @litchar{|} and
under an implicit @litchar{:} is an @tech{alt-block}.

A @litchar{|} that starts the enclosing block can appear at the start of
a line with new indentation. The following four groups are the same:

@(rhombusblock:
    hello
    | world
    | universe

    hello
      | world
      | universe

    hello | world
          | universe

    hello |
            world
          |
            universe
  )

Each of the four groups has two elements: @litchar{hello} and a block.
The block has two groups, each of which is a more nested block. The
first nested block has @litchar{world} in a single group, and the second
nested block as @litchar{universe} in a single group.

A @litchar{|} cannot be a in a top-level sequence of groups, and it
cannot appear just after @litchar{:}, but a @litchar{|} can start a
group immediately within @litchar{()}, @litchar{[]}, or @litchar{{}}.
Like @litchar{:}, the content of a block after @litchar{|} cannot be
empty unless explicit-grouping @litchar{«} and @litchar{»} are used.

If a @litchar{|} appears on the same line as an earlier @litchar{|} and
is not more nested inside @litchar{()}, @litchar{[]}, or @litchar{{}},
then the @litchar{|} terminates the earlier @litchar{|}'s block and
continues its enclosing block with a new @litchar{|} group. The intent
and consequence of this rule is that multiple @litchar{|}s can be used
on a single line as an alternative to starting each @litchar{|} on its
own line, making the following groups the same as the above groups:

@(rhombusblock:
    hello | world | universe

    hello
    | world | universe
)

The implicit shifting of @litchar{|} by half a column is consistent with its
visual representation, and it avoids the possibility of a group
sequence that contains a mixture of @litchar{|}-started groups and other kinds
of groups. Standard indentation uses no additional space of
indentation before @litchar{|} relative to its enclosing block's group.


@section[~tag: "semicolon"]{Separating Groups with @litchar{;} and @litchar{,}}

A @litchar{;} separates two groups on the same line. A @litchar{;} is
allowed in any context—except between groups immediately within,
@litchar{()}, @litchar{[]}, or @litchar{{}}, where a @litchar{,}
separates groups. The following three blocks are the same:

@(rhombusblock:
    hello:
      world
      universe  

    hello:
      world; universe

    hello: world; universe
  )

The @litchar{;} and @litchar{,} separators interact differently with blocks formed by
@litchar{:} and @litchar{|}. A @litchar{,} closes subgroups and blocks as necessary to reach
an enclosing @litchar{()}, @litchar{[]}, or @litchar{{}}, while a @litchar{;} separates groups within a
nested group sequence. If @litchar{;} would create an empty group, it is
ignored.

For example, the following two groups are the same, and they have one
parenthesized term that has a single block, and the block has two
groups:

@(rhombusblock:
    (hello: world; universe)

    (hello: world
            universe)
  )

The following two groups are also the same, where the group has one
parenthesized term, but that term contains two groups, where the first
group is a block that contains a single group:


@(rhombusblock:
    (hello: world, universe)

    (hello: world,
     universe)
  )

@section[~tag: "guillemot"]{Line- and Column-Insensitivity with @litchar{«} and @litchar{»}}

A block can be delimited explicitly with @litchar{«} and @litchar{»} to
disable the use of line and column information for parsing between
@litchar{«} and @litchar{»}. A @litchar{«} can be used immediately after
@litchar{:} or immediately after @litchar{|}, in which case a
@litchar{»} indicates the end of the block that starts after the
@litchar{:} or @litchar{|}. Within the block, an explicit @litchar{;}
must be used to separate groups. A @litchar{«} can also be used
immediately after @litchar{'} and then @litchar{»} is used just before
the closing @litchar{'}, but that is a different kind of @litchar{«}
@litchar{»} that is specific to supporting nested @litchar{'} pairs.

A sequence of groups, either at the top level or within a block, can be
written without line and column sensitivity as @litchar{;} followed
immediately by @litchar{«}, in which case a @litchar{»} indicates the
end of the sequence, and groups within the sequence are separated by
@litchar{;}. When parsing, the groups within the sequence are spliced
into the enclosing context. The combination of @litchar{;} and
@litchar{«} is intended for entering line- and column-insensitive mode
for a single group or for representing a sequence of groups that is not
within a block.

Whitespace and block comments are allowed between a @litchar{:},
@litchar{|}, or @litchar{;} and its @litchar{«}, but in a line-sensitive
context, the @litchar{«} must be on the same line as its @litchar{:},
@litchar{|}, or @litchar{;}.

The following five groups are the same:

@(rhombusblock:
    hello:
      if x
      | world
        planet
      | universe

    hello: if x | world; planet | universe

    hello:«
      if x
      |« world;
         planet »
      |« universe »»

    hello:« if x |« world; planet » |« universe »»

    ;«hello
      :
      «
      if
      x
      |
      «
      world
      ;
      planet
      »
      |
      «
      universe
      »
      »
      »
  )

Using @litchar{«} and @litchar{»} can “armor” a shrubbery for transport from one
context to another where its line breaks or indentation might get
mangled. For example, an editor might offer an operation to armor a
range of text in perparation for moving or copying the text, and then
it can be properly indentend in its destination before unmarmoring.
Along similar lines, when writing code as data to be read back later,
it's easy for a printer to insert explicit @litchar{«} and @litchar{»}.

In rare cases, a programmer might write @litchar{«} and @litchar{»} directly. Although
many shrubbery forms can be written with @litchar{:}, @litchar{|}, and @litchar{;} on a single
line, as illustrated above, not all forms can be collapsed to a single
line without extra delimiters. For example, these six groups are all
different:

@(rhombusblock:
    outside:
      inside: fruit
      rind

    // not the same, because `rind` is within `inside:`
    outside: inside: fruit; rind

    if true
    | if false
      | x
      | y
    | z

    // not the same, because there's one block with five `|` alternatives
    if | true | if false | x | y | z

    hello:
      if x
      | world
      | universe
      the end

    // not the same, because `the end` is in the second `|`:
    hello: if x | world | universe; the end
  )

Using @litchar{«} and @litchar{»} can help in those cases:

@(rhombusblock:
    outside:
      inside: fruit
      rind

    outside: inside:« fruit »; rind

    if true
    | if false
      | x
      | y
    | z

    if | true |« if false | x | y » | z

    hello:
      if x
      | world
      | universe
      the end

    hello: if x | world |« universe »; the end
)

Even so, delimiting blocks with @litchar{«} and @litchar{»} is expected to be rare in
practice, both because programmers are likely to break things across
lines and because a language that uses shrubbery notation is likely to
allow @litchar{()} in places where grouping might be needed. For example,
assuming that @litchar{if} is an expression form and @litchar{()} can wrap an
expression, a nested conditional is probably better written like this:

@(rhombusblock:
    if | true | (if false | x | y) | z
)

Using @litchar{()} in this way does not produce an equivalent shrubbery to

@(rhombusblock: if | true |« if false | x | y »| z)

but it might represent an equivalent expression in the language using
shrubbery notation.

To stay consistent with blocks expressed through line breaks and
indentation, a block with @litchar{«} and @litchar{»} must still appear at the end of
its enclosing group.

@verbatim[~indent: 2]{
// not allowed, because a block must end a group
inside:« fruit » more
}


@section[~tag: "continuing-backslash"]{Continuing a Line with @litchar{\}}

As a last resort, @litchar{\} can be used at the end of a line (optionally
followed by whitespace and coments on the line) to continue the next
line as it if were one line continuing with the next line. The itself
@litchar{\} does not appear in the parsed form. A that is not at the end of a
line (followed by whitespace and coments) is treated the same as
whitespace.

Lines contianing only whitespace and (non-term) comments do not count
as “the next line” even for @litchar{\} continuations, so any number of
whitespace and comment lines can appear between @litchar{\} and the line that
it continues.

@(rhombusblock:
    this is \
      the first group
    this \ is \ the \ second \ group

    this is a group \
      with (a,
                                 nested,
                                 list)

    this is a group \
     with (a,
                    \
           nested,
                    \
           list)

    this is a group \
     with (a,
                    \
           /* this a comment on `nested`: */
           nested,
                    \
           list)
)

@section[~tag: "group-comment"]{Group Comments with @litchar{#//}}

A @litchar{#//} comments out a group or @litchar{|} alternative. To comment out a
group, @litchar{#//} must appear either on its own line before a group or at
the start of a group. To comment out an alternative, @litchar{#//} must appear
on its own line before the alternative or just before a @litchar{|} that does
@emph{not} start a new line.

The interaction between @litchar{#//} and indentation depends on how it is
used:

@itemlist[

 @item{When @litchar{#//} appears completely on its own line (possibly with
   whitespace and non-group comments), then its indentation does not
   matter. It comments out the next group or alternative—which might
   be a single-line group, block, or @litchar{|} block.},

 @item{When @litchar{#//} appears at the start of a group with more tokens
   afterward on the same line, it determines that group's indentation,
   and it must obey any constraints on the group's indentation. When
   @litchar{#//} appears immediately after an opener but with nothing else
   afterward on the same line, it determines indentation for the
   groups immediately within the opener, and it comments out the first
   group.},

 @item{When @litchar{#//} appears just before a @litchar{|} on the same line, then unlike
   the case for groups, it does not affect the the column of the @litchar{|}
   as used to align alternatives on later lines. (That's because the
   half-column alignment of @litchar{|} does not fit with the column alignment
   of @litchar{#}.) Along those lines and to avoid an indentation mismatch, a
   @litchar{#//} is not allowed to start a line for commenting out a @litchar{|}
   alternative on the same line.}

]

A @litchar{#//} is not allowed without a group or alternative afterward to
comment out. Multiple @litchar{#//}s do not nest (i.e., two @litchar{#//}s in a row is
always an error).

The following three groups all parse the same:

@(rhombusblock:
    {
      hello:
        val x: f(1, 2 + 3)
        match x
        | 1: 'one'
        | 2: 'two'
    }

    {
      hello:
        val x:
          #//
          g(-1)
          f(
            #//
            0,
            1,
            2 + 3,
            #//
            4 + 5)
        #//
        not included in the code
        match x
        #//
        | 0: no
        | 1: 'one'
        #//
        | 1.5: no
        | 2: 'two'
        #//
        | 3: no,
      #//
      goodbye:
        the enclosing group of the block is commented out
    }

    {
      hello:
        val x:
          #// g(-1)
          f(#// 0, 1, 2 + 3, #// 4 + 5)
        #// not included in the code
        match x #// | 0: no | 1: 'one' #// | 1.5: no
                    | 2: 'two' #// | 3: no,
      #// goodbye:
        the enclosing group of the block is commented out
    }
  )


@include_section["at-notation.scrbl"]
