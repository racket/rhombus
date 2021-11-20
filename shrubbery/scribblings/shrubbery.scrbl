#lang scribble/rhombus/manual

@(import:
    scribble/bnf:
      rename: #{BNF-seq} ~to bseq
              optional ~to boptional
      expose: nonterm bseq kleenestar kleeneplus boptional
    "grammar-s-exp.rkt":
      expose: shrubbery_s_expression_grammar)

@(def bis: @elem{@hspace[1]::=@hspace[1]})
@(def bor: @elem{@hspace[1] | @hspace[1]})

@(def opener: @emph{opener})
@(def opener_closer: @elem{@opener--@italic{closer}})

@title{Shrubbery Notation}

Shrubbery notation is similar to S-expression notation, but instead of
generating fully formed trees, it is intended to partially group input
for further enforestation by another parser (e.g., as in Honu). The
notation is line- and indentation-sensitive, and the parsed form of a
shrubbery imposes grouping to ensure that further parsing is consistent
with the shrubbery's lines and indentation.

@table_of_contents[]

@section{Motivation}

S-expression notation imposes a grouping at the lexeme level that is all
but guaranteed to be respected by further parsing via macro expansion.
One consequence of this lexeme-based grouping is that programs can be
pretty-printed and textually traversed in standard ways.

A traditional use of S-expression notation, however, insists that
@emph{all} grouping is reflected in the S-expression. Reifying all
grouping at the lexeme level is so onerous that many practical
deployments of S-expressions include deviations from the rule, such as
keyword-based arguments or implicit grouping by position (as in various
Clojure forms).

Another disadvantage of S-expressions is that many of the parentheses
are redundant after the expression is pretty-printed, because
indentation provides the same grouping information in a more
human-readable way. That observation suggests instead relying on line
breaks and indentation to impart grouping information, as in Python.

Shrubbery notation explores a point in the design space where the
notation is

@itemlist[

 @item{line- and indentation-sensitive, and},
 
 @item{intended to constrain grouping but not reflect every detail of
  grouping.}

]

Deferring complete grouping to another parser relieves a burden on
reader-level notation. At the same time, line- and indentation-sensitive
rules constrain parsing to ensure that line breaks and indentation in
the source are not misleading.

@section{Examples}

Here are some example shrubberies. Each line either uses old indentation
to continue a nesting level that was started on a previous line, starts
with new indentation and follows a line that ends with @litchar{:}, or
starts with new indentation and a @litchar{|} on the same line. A
@litchar{:} or @litchar{|} can also appear in the middle of a line, but
that's roughly a shorthand for starting a new indented line after the
@litchar{:} or before the @litchar{|}. The complete rules involve more
terminology, but that's enough to get a sense of the examples.

@(rhombusblock:
    def identity(x): x

    def fib(n):
      cond
      | n == 0: 0
      | n == 1: 1
      | else: fib(n-1) + fib(n-2)

    def print_sexp(v):
      match v
      | empty: display("()")
      | cons(a, d):
          if is_list(d)
          | display("(")
            print_sexp(a)
            for (v = in_list(d)):
              display(" ")
              print_sexp(v)
            display(")")
          | display("(")
            print_sexp(a)
            display(". ")
            print_sexp(d)
            display(")")
      | v: print_atom(v)
  )

Forms like @litchar{def}, @litchar{cond}, and @litchar{match} are not
specified by shrubbery notation, since specifying those forms is up to a
language that is built on top of shrubbery notation. Still, shrubbery
notation is meant to accommodate a particular kind of syntax for nested
blocks (via @litchar{:} and indentation) and conditional branches (via
@litchar{|}).

Identifiers are C-style with alphanumerics and underscores. Operators
are sequences of symbolic characters in the sense of
@litchar{char-symbolic?}, roughly. No spaces are needed between
operators and non-operators, so @litchar{1+2} and @litchar{1 + 2} mean
the same thing. Comments are C-style, plus a @litchar{#//} group-comment
form. See @secref["lexeme-parsing"] for more information.

The following tokens are used for grouping, in addition to line breaks
and indentation:

@verbatim[~indent: 2]{
( ) [ ] { }  ; ,   : |   « »  \
}

Parentheses, square brackets, and curly braces are used to form groups
in the obvious way. A @litchar{;} or @litchar{,} acts as a group
separator, even within a single line. A @litchar{:} or @litchar{|}
treats remaining item on the same line like a new indented line, which
forms a subgroup. A guillemet pair @litchar{«} and @litchar{»} can be
used (probably very rarely) to explicitly bracket subgroups formed by
@litchar{:} and @litchar{|} without line breaks. A @litchar{\} continues
a line, effectively shifting all columns on the next line as if they
appeared immediately after the @litchar{\}.

@section{Grouping by Lines}

The main grouping rule is that sequences on different lines with the
same indentation create separate groups, one for each line.

@(rhombusblock:
    this is the first group
    this is the second group
  )

Comments and lines with only whitespace are ignored. They don't count
when this document says “the previous line” or “the next line.”

@section{Grouping by Opener--Closer Pairs}

An @opener_closer pair @litchar{(} and @litchar{)},
@litchar{[} and @litchar{]}, or @litchar["{"] and @litchar["}"] forms a
nested group that can span lines. Within the @opener_closer
pair, @litchar{,} sparates groups. Groups can be on separate lines at
the same indentation, but groups on separate lines still must be
separated by @litchar{,}. Parsing retains whether a subgroup is formed
by @litchar{()}, @litchar{[]}, or @litchar{{}}.

@(rhombusblock:
    group 1
    [group 2 - subgroup I,
     group 2 - subgroup II,
     (group 2 - subgroup III - subsubgroup A,
      group 2 - subgroup III - subsubgroup B,
      {group 2 - subgroup III - subsubgroup C, subsubsubgroup α,
       group 2 - subgroup III - subsubgroup C, subsubsubgroup β})]
    (group 3 - subgroup I,  group 3 - subgroup II,
     group 3 - subgroup III)
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

A trailing @litchar{,} is only standard style when the _closer_ that follows is
on its own line.

@(rhombusblock:
    list(
      red,
      green,
      blue,
      orange,
    )
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
it starts a new sequence of groups that form a _block_:

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

A block that is started with @litchar{:} normally cannot be empty (unless
explicit-grouping @litchar{«} and @litchar{»} are used as described in a later
section), so the following is ill-formed:

@verbatim[~indent: 2]{
bad_empty:  // empty block disallowed
}

However, @litchar{:} can be used at the start of a group so that the group
contains only a block. When @litchar{:} starts a group that is in the
top-level sequence or within an @opener_closer pair, the block
created by @litchar{:} is allowed to be empty (because that provides a way to
express an empty in a context where it likely to be intentional
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
preceding line does _not_ end with @litchar{:}, then the indented line
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
nested), and it is implicitly followed by a @litchar{:} that conceptually
occupies same column as the @litchar{|}. That is, like @litchar{:}, a @litchar{|} always
creates a nested block. Furthermore, @litchar{|} starts an enclosing block
that includes the @litchar{|} block plus subsequent @litchar{|} blocks that are at the
same indentation. A @litchar{|} that starts the enclosing block can appear at
the start of a line with new indentation. The following four groups
are the same:

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

A @litchar{|} cannot be a in a top-level sequence of groups or start a
group immediately within @litchar{()}, @litchar{[]}, or @litchar{{}},
and it cannot appear just after @litchar{:}. Like @litchar{:}, the
content of a block after @litchar{|} cannot be empty unless
explicit-grouping @litchar{«} and @litchar{»} are used.

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
an enclosing @litchar{()}, @litchar{[]}, or @litchar{{}}, while a @litchar{;} separate groups within a
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
must be used to separate groups.

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

    // not the same, because @litchar{rind} is within @litchar{inside:}
    outside: inside: fruit; rind

    if true
    | if false
      | x
      | y
    | z

    // not the same, because there's one block with five @litchar{|} alternatives
    if | true | if false | x | y | z

    hello:
      if x
      | world
      | universe
      the end

    // not the same, because @litchar{the end} is in the second @litchar{|}:
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

Using @litchar{()} in this way does not produce an equivalent shrubbery to `if
| true |« if false | x | y »| z`, but it might represent an equivalent
expression in the language using shrubbery notation.

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
*not* start a new line.

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

@section{More Examples}

Here are more example shrubberies. These shrubberies are not
necessarily consistent with each other in the sense of sketching a
single language that uses shrubbery notation; they show different
potential ways of using the notation.

@(rhombusblock:
    def pi: 3.14

    def
    | fib(0): 0
    | fib(1): 1
    | fib(n): fib(n-1) + fib(n-2)

    def fib(n):
      match n
      | 0: 0
      | 1: 1
      | n: fib(n-1) + fib(n-2)

    def fib(n):
      match n | 0: 0
              | 1: 1
              | n: (fib(n-1)
                      + fib(n-2))

    def fib(n):
      match n
      | 0:
          0
      | 1:
          1
      | n:
          fib(n-1) + fib(n-2)

    def make_adder(n):
      lambda (m):
        printf("adding to ~a\n", m)

    def fourth(n: integer):
      def m: n*n
      def v: m*m
      printf("~a^4 = ~a\n", n, v)
      v

    struct posn(x, y):
      property prop_equal_and_hash:
        let (hc = lambda (a: posn, hc):
                    hc(a.x) + hc(a.y),
             eql = lambda (a: posn, b: posn, eql):
                     eql(a.x, b.x) && eql(a.y, b.y)):
          values(eql, hc, hc)

    def go():
      def helper(n):
        list(n, n)
      def more(m):
        if m == 0 | "done"
                  | more(m - 1)
      helper(more(9))

    def curried:
      fun (x):
        fun (y):
          fun (z):
            [x, y, z]

    let (x = 1,
         y = 2):
      printf("About to add")
      x+y

    def show_zip(l, l2):
      for (x = in_list(l),
           x2 = in_list(l2)):
        print(x)
        print_string(" ")
        print(x2)
        newline()

    def show_combos(l, l2):
      for (x = in_list(l)):
       then (x2 = in_list(l2)):
         print(x)
         print_string(" ")
         print(x2)
         newline()
)

@section{Parsed Representation}

The parse of a shrubbery can be represented by an S-expression:

@itemlist[

 @item{Each group is represented as a list that starts @litchar{'group}, and
   the rest of the list are the elements of the group.},

 @item{Atom elements are represented as “themselves” within a group,
   including identifers a symbols, except that an operator is
   represented as a 2-list that is @litchar{'op} followed by the operator name
   as a symbol.},

 @item{A group sequence is represented as a list of @litchar{'group} lists.},

 @item{An element created by @litchar{()} is represented by @litchar{'parens} consed
   onto a group-sequence list.},
   
 @item{An element created by @litchar{[]} is represented by @litchar{'brackets} consed
   onto a group-sequence list.},

 @item{An element created by @litchar{{}} is represented by @litchar{'braces} consed
   onto a group-sequence list.},

 @item{A block is represented as either @litchar{'block} or @litchar{'alts} consed onto a
   group-sequence list. The representation uses @litchar{'alts} if the content
   of the block is a squence of groups started with @litchar{|}, and it's
   @litchar{'block} otherwise.},

 @item{A block created to follow @litchar{|} appears immediately in an @litchar{'alts}
   list.}

]

Note that a block can only appear immediately in a @litchar{'group} or @litchar{'alts}
list. Note also that there is no possibility of confusion between
symbol atoms in the input and @litchar{'group}, @litchar{'block}, etc., at the start
of a list in an S-expression representation, because symbol atoms will
always appear as non-initial items in a @litchar{'group} list.

Overall, the grammar of S-expression representations is as follows:

@nested[~style: symbol(inset), shrubbery_s_expression_grammar]

Here's the same grammar, but expressed using Rhombus constructors:

@nested[~style: symbol(inset),
        bnf.BNF([@nonterm{parsed},
                 @rhombus[[symbol(top), $$(@nonterm{group}), ...]]],
                [@nonterm{group},
                 @rhombus[[symbol(group), $$(@nonterm{term}), ...]]],
                [@nonterm{term},
                 @nonterm{atom},
                 @rhombus[[symbol(op), $$(@nonterm{symbol})]],
                 @rhombus[[symbol(parens), $$(@nonterm{group}), ...]],
                 @rhombus[[symbol(brackets), $$(@nonterm{group}), ...]],
                 @rhombus[[symbol(braces), $$(@nonterm{group}), ...]],
                 @nonterm{block},
                 @rhombus[[symbol(alts), $$(@nonterm{block}), ...]]],
                [@nonterm{block},
                 @rhombus[[symbol(block), $$(@nonterm{group}), ...]]])]

Here are some example shrubberies with their S-expression parsed
representations:

@verbatim[~indent: 2]{
def pi: 3.14

(group de pi (block (group 3.14)))

def fourth(n: integer):
  def m: n*n
  def v: m*m
  printf("~a^4 = ~a\n", n, v)
  v

(group def
       fourth
       (parens (group n (block (group integer))))
       (block
        (group def m (block (group n (op *) n)))
        (group def v (block (group m (op *) m)))
        (group printf
               (parens (group "\"~a^4 = ~a\\n\"") (group n) (group v)))
        (group v)))

if x = y
| same
| different

(group if x (op =) y (alts (block (group same))
                           (block (group different))))

define fib(n):
  match n
  | 0: 0
  | 1: 1
  | n: fib(n-1) + fib(n-2)

(group define
       fib
       (parens (group n))
       (block
        (group match
               n
               (alts
                (block (group 0 (block (group 0))))
                (block (group 1 (block (group 1))))
                (block
                 (group n
                        (block
                         (group fib
                                (parens (group n (op -) 1))
                                (op +)
                                fib
                                (parens (group n (op -) 2))))))))))))
}

@section[~tag: "lexeme-parsing"]{Lexeme Parsing}

The tokens used for grouping and indentation are distinct lexemes:

@verbatim[~indent: 2]|{
( ) [ ] { }   ; ,   : |   « »  \
}|

Other lexemes are described by the grammar in the table below, where an
asterisk in the left column indicates the productions that correspond to
lexemes. Only simple forms of numbers are supported directly (decimal
integers, decimal floating point, and hexadecimal integers, in all cases
allowing @litchar{_}s between digits), but a
@litchar["#{"]...@litchar["}"] escape provides access to the full Racket
S-expression number grammar. Boolean literals are Racket-style, instead
of reserving identifiers. Special floating-point values similarly use a
@litchar{#} notation.

Identifiers are formed from Unicode alphanumeric characters plus @litchar{_},
where the initial character must not be a numeric character. An
identifier prefixed with @litchar{~} forms a keyword, analogous to prefixing an
identifier with @litchar{#:} in Racket.

Operators are formed from Unicode symbolic and punctuation characters
other than the ones listed above as distinct lexemes (plus a few more,
like @litchar{"} and @litchar{'}), but @litchar{|} or @litchar{:} is
also allowed in an operator name as long as it is not by itself. A
multi-character operator cannot end in @litchar{+}, @litchar{-}, or
@litchar{.} to avoid ambiguity in cases like @litchar{1+-2} (which is
@litchar{1} plus @litchar{-2}, not @litchar{1} and @litchar{2} combined
with a @litchar{+-} operator), unless the operator contains only
@litchar{+}, @litchar{-}, or @litchar{.} (so @litchar{++}, @litchar{--},
and @litchar{...} are allowed). Also, multi-character operator cannot
end with @litchar{/} or contain @litchar{//} or @litchar{/*}, because
that can create ambiguities with comments.

Implicit in the grammar is the usual convention of choosing the largest
possible match at the start of a stream. Not reflected in the grammar is
a set of delimiter requirements: numbers, @litchar{#true}, and
@litchar{#false} must be followed by a delimiter. For example,
@litchar{1x} is a lexical error, because the @litchar{x} after
@litchar{1} is not a delimiter. Non-alphanumeric characters other than
@litchar{_} and @litchar{.} are delimiters. Finally, the treatment of
@litchar{+} and @litchar{-} as a number prefix versus an operator is
subject to a special rule: they are parsed as operators when immediately
preceded by an alphanumeric character, @litchar{_}, @litchar{)},
@litchar{]}, or @litchar["}"] with no whitespace in between. For
example, @litchar{1+2} is @litchar{1} plus @litchar{2}, but @litchar{1
 +2} is @litchar{1} followed by the number @litchar{+2}.

When a @litchar["#{"]...@litchar["}"] escape describes an identifier
S-expression, it is an identifier in the same sense as a
shrubbery-notation identifier. the same holds for numbers, booleans,
strings, byte strings, and keywords. A @litchar["#{"]...@litchar["}"]
escape must _not_ describe a pair, because pairs are used to represent a
parsed shrubbery, and allowing pairs would create ambiguous or
ill-formed representations.

A @litchar["@"] starts an at-expression form similar to the notaton
supported by @litchar{#lang at-exp} (which oriented toward S-expressions
and readtable-based). The next subsection explains in more detail, but
the table below sketches the shape of @litchar["@"] forms.

@(def is_lex: @elem{★@hspace[1]})
@(def no_lex: "")
@(def empty_line: ["", @hspace[1], "", "", ""])

@tabular[
  [
    [is_lex, @nonterm{identifier}, bis, bseq(@nonterm{alpha}, kleenestar(@nonterm{alphanum})), ""],
    empty_line,
    [no_lex, @nonterm{alpha}, bis, @elem{@italic{an alphabetic Unicode character or} @litchar{_}}, ""],
    empty_line,
    [no_lex, @nonterm{alphanum}, bis, @nonterm{alpha}, ""],
    ["", "", bor, @italic{a numeric Unicode character}, ""],
    empty_line,
    [is_lex, @nonterm{keyword}, bis, bseq(@litchar{~}, @nonterm{alpha}), ""],
    empty_line,
    [is_lex, @nonterm{operator}, bis, bseq(kleenestar(@nonterm{opchar}), @nonterm{tailopchar}),
     @elem{@italic{not} @litchar{|} @italic{or} @litchar{:} @italic{...}}],
    ["", "", bor, kleeneplus(@litchar{.}), @elem{@italic{... or containing} @litchar{//} @italic{...}}],
    ["", "", bor, kleeneplus(@litchar{+}), @elem{@italic{... or containing} @litchar{/*}}],
    ["", "", bor, kleeneplus(@litchar{-}), ""],
    empty_line,
    [no_lex, @nonterm{opchar}, bis, @elem{@italic{a symbolic Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{a punctuation Unicode character not in} @nonterm{special}}, ""],
    ["", "", bor, @elem{@italic{one} @litchar{:} @litchar{|}}, ""],
    empty_line,
    [no_lex, @nonterm{tailopchar}, bis, @elem{@italic{anything in} @nonterm{opchar} @italic{except}
                                              @litchar{+}, @litchar{-}, @litchar{.}, @litchar{/}}, ""],
    empty_line,
    [no_lex, @nonterm{special}, bis, @elem{@italic{one of} @litchar{(}, @litchar{)}, @litchar{[},
                                           @litchar{]}, @litchar["{"], @litchar["}"], @litchar{«}, @litchar{»}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{"}, @litchar{;}, @litchar{,}, @litchar{~}, @litchar{#},
                        @litchar{\}, @litchar{_}, @litchar["@"]}, ""],
    empty_line,
    [is_lex, @nonterm{number}, bis, @nonterm{integer}, ""],
    ["", "", bor, @nonterm{float}, ""],
    ["", "", bor, @nonterm{hexinteger}, ""],
    empty_line,
    [no_lex, @nonterm{integer}, bis, bseq(boptional(@nonterm{sign}), @nonterm{nonneg}), ""],
    empty_line,
    [no_lex, @nonterm{sign}, bis, @elem{@italic{one of} @litchar{+} @italic{or} @litchar{-}}, ""],
    empty_line,
    [no_lex, @nonterm{nonneg}, bis, bseq(@nonterm{decimal}, kleeneplus(@nonterm{usdecimal})), ""],
    empty_line,
    [no_lex, @nonterm{decimal}, bis, @elem{@italic{one of} @litchar{0} @italic{through} @litchar{9}}, ""],
    empty_line,
    [no_lex, @nonterm{usdecimal}, bis, @nonterm{decimal}, ""],
    ["", "", bor, bseq(@litchar{_}, @nonterm{decimal}), ""],
    empty_line,
    [no_lex, @nonterm{float}, bis, bseq(boptional(@nonterm{sign}),
                                        @nonterm{nonneg},
                                        @litchar{.},
                                        boptional(@nonterm{nonneg}),
                                        boptional(@nonterm{exp})), ""],
    ["", "", bor, bseq(boptional(@nonterm{sign}),
                       @litchar{.},
                       @nonterm{nonneg},
                       boptional(@nonterm{exp})), ""],
    ["", "", bor, bseq(boptional(@nonterm{sign}),
                       @nonterm{nonneg},
                       @nonterm{exp}), ""],
    ["", "", bor, @litchar{#inf}, ""],
    ["", "", bor, @litchar{#neginf}, ""],
    ["", "", bor, @litchar{#nan}, ""],
    empty_line,
    [no_lex, @nonterm{exp}, bis, bseq(@litchar{e},
                                      boptional(@nonterm{sign}),
                                      @nonterm{nonneg}), ""],
    ["", "", bor, bseq(@litchar{E},
                       boptional(@nonterm{sign}),
                       @nonterm{nonneg}), ""],
    empty_line,
    [no_lex, @nonterm{hexinteger}, bis, bseq(@litchar{0x},
                                             @nonterm{hex},
                                             kleenestar(@nonterm{ushex})), ""],
    empty_line,
    [no_lex, @nonterm{hex}, bis, @elem{@italic{one of} @litchar{0} @italic{through} @litchar{9}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{a} @italic{through} @litchar{f}}, ""],
    ["", "", bor, @elem{@italic{one of} @litchar{A} @italic{through} @litchar{F}}, ""],
    empty_line,
    [no_lex, @nonterm{ushex}, bis, @nonterm{hex}, ""],
    ["", "", bor, bseq(@litchar{_}, @nonterm{hex}), ""],
    empty_line,
    [is_lex, @nonterm{boolean}, bis, @litchar{#true}, ""],
    ["", "", bor, @litchar{#false}, ""],
    empty_line,
    [is_lex, @nonterm{string}, bis, bseq(@litchar{"}, kleenestar(@nonterm{strelem}), @litchar{"}), ""],
    empty_line,
    [no_lex, @nonterm{strelem}, bis, @italic{like Racket, but no literal newline}, @elem{@litchar{\U} ≤ 6 digits}],
    empty_line,
    [is_lex, @nonterm{bytestring}, bis, bseq(@litchar{#"}, kleenestar(@nonterm{bytestrelem}), @litchar{"}), ""],
    empty_line,
    [no_lex, @nonterm{bytestrelem}, bis, @italic{like Racket, but no literal newline}, ""],
    empty_line,
    [is_lex, @nonterm{sexpression}, bis, bseq(@litchar["#{"], @nonterm{racket}, @litchar["}"]), ""],
    empty_line,
    [no_lex, @nonterm{racket}, bis, @italic{any non-pair Racket S-expression}, ""],
    empty_line,
    [is_lex, @nonterm{comment}, bis, bseq(@litchar{//}, kleenestar(@nonterm{nonnlchar})), ""],
    ["", "", bor, bseq(@litchar{/*}, kleenestar(@nonterm{anychar}), @litchar{*/}), "nesting allowed"],
    empty_line,
    [no_lex, @nonterm{nonnlchar}, bis, @italic{any character other than newline}, ""],
    empty_line,
    [is_lex, @nonterm{atexpression}, bis, bseq(@litchar["@"],
                                               boptional(@nonterm{command}),
                                               boptional(@nonterm{arguments}),
                                               boptional(@nonterm{body})), "no space between parts"],
    empty_line,
    [no_lex, @nonterm{command}, bis, @nonterm{identifier}, ""],
    ["", "", bor, @nonterm{keyword}, ""],
    ["", "", bor, @nonterm{operator}, ""],
    ["", "", bor, @nonterm{number}, ""],
    ["", "", bor, @nonterm{boolean}, ""],
    ["", "", bor, @nonterm{string}, ""],
    ["", "", bor, @nonterm{bytestring}, ""],
    ["", "", bor, @nonterm{racket}, ""],
    ["", "", bor, bseq(@litchar{(}, @kleenestar(@nonterm{group}), @litchar{)}), ""],
    ["", "", bor, bseq(@litchar{«}, @nonterm{group}, @litchar{»}), ""],
    empty_line,
    [no_lex, @nonterm{arguments}, bis, bseq(@litchar{[}, @kleenestar(@nonterm{group}), @litchar{]}),
     @italic{usual @litchar{,}-separated}],
    empty_line,
    [no_lex, @nonterm{body}, bis, bseq(@litchar["{"], @nonterm{text}, @litchar["}"]),
     @elem{@italic{escapes in} @nonterm{text}}],
    ["", "", bor, bseq(@nonterm{atopen}, @nonterm{text}, @nonterm{atclose}),
     @elem{@nonterm{atclose} @italic{match} @nonterm{atopen}}],
    empty_line,
    [no_lex, @nonterm{atopen}, bis, bseq(@litchar{|}, kleenestar(@nonterm{asciisym}), @litchar["{"]), ""],
    empty_line,
    [no_lex, @nonterm{atclose}, bis, bseq(@litchar["}"], kleenestar(@nonterm{asciisym}), @litchar{|}),
     @italic{flips paren-line}],
    
  ]
]

@section{At-Notation Using @litchar["@"]}

An @litchar["@"] form of the shape

@verbatim[~indent: 2]|{
 @«|@italic{command} ...»[@|italic{arg}, ...]{ |@italic{body} }...
}|

is parsed into the same representation as

@verbatim[~indent: 2]{
    @italic{command} ...(@italic{arg}, ..., [@italic{parsed_body}, ...], ...)
}

That is, the command part is left at the front and spliced into its
enclosing group, while the argument and body parts are wrapped with
parentheses to make them like arguments. Each body text is parsed into
a list of string literals and escapes, and multiple body texts can
be provided in multiple @litchar["{"]...@litchar["}"]s.

The command part usually does not have @litchar{«»}, and it is instead
usually written as an identifier, operator, or parenthesized term. The
argument and body parts, when present, always use @litchar{[]} and @litchar{{}},
respectively. Any of the three kinds parts can be omitted, but when
multiple parts are present, they must have no space between them or
the leading @litchar["@"]. When the argument and body parts are both
omitted, the command part is simply spliced into its context.

The conversion to a call-like form, keeping each body in a separate
list, and allowing multiple body arguments are the three main ways
that shrubbery @litchar["@"] notation differs from @litchar{#lang at-exp} notation. The
other differences are the use of @litchar{«}...@litchar{»} instead of @litchar{|}...@litchar{|} for
delimiting a command, and the use of @litchar["@//"] instead of @litchar["@;"] for
comments. The details are otherwise meant to be the same, and the rest
of this section is mostly a recap.

A body part is treated as literal text, except where @litchar["@"] is
used in a body to escape. An unescaped @litchar["}"] closes a body,
except that an unescaped @litchar["{"] must be balanced by an unescaped
@litchar["}"], with both treated as part of the body text. Instead of
@litchar["{"], a body-starting opener can be @litchar{|} plus
@litchar["{"] with any number of ASCII punctuation and symbol characters
(other than @litchar["{"]) in between; the corresponding closer is then
the same sequence in reverse, except that some characters are flipped:
@litchar["{"] to @litchar["}"], @litchar{(} to @litchar{)}, @litchar{)}
to @litchar{(}, @litchar{[} to @litchar{]}, @litchar{]} to @litchar{[},
@litchar{<} to @litchar{>}, and @litchar{>} to @litchar{<}. With an
@litchar{|}...@litchar["{"] opener, an escape is formed by using the
opener followed by @litchar["@"], while opener–closer pairs balance
within the body text. When multiple body parts are provided, each can
use a different opener and closer. The parsed form of the body breaks up
the body text into lines and @litchar{"\n"} as separate string literals
in the parsed list form, with each escape also being its own element in
the list form. Parsed body text also has leading and trailing whitespace
adjusted the same as with @litchar{#lang at-exp}.

After the @litchar["@"] of an escape in body text, the escape has the
same form as an at-notaton form that starts with @litchar["@"] as a
shubbery. That is, @litchar["@"] forms are essentially the same whether
starting in shrubbery mode or body-text mode.

In body text, there are two additional comment forms that are not
supported in shrubbery mode. A @litchar["@//{"] starts a block comment
that ends with @litchar["}"], and the comment form is not part of the
body text. The @litchar["@//"] comment form must be prefixed with an
opener when its enclosing body is started with an opener that isn't just
@litchar["{"], and the @litchar["{"] after @litchar["@//"] can more
generally be an @litchar{|}...@litchar["{"] opener with the
corresponding closer. Opener–closer pairs must be balanced in the
commented block, the same as in body text. A @litchar["@//"] comment
form (prefixed with an opener as needed to form an escape) that is not
followed by @litchar["{"] or an @litchar{|}...@litchar["{"] opener
comments out the rest of the line, including a comment-terminating
newline.

@section{Rationale}

The lexeme-level syntax is chosen to be familiar to programmers
generally. The sequence @litchar{1+2} is one plus two, not a strangely
spelled identifier. Tokens like @litchar{(}, @litchar{,}, @litchar["{"]
and @litchar{;} are used in familiar ways. Shrubbery notation provides
enough grouping structure that code navigation and transformation should
be useful and straightforward in an editor.

Parentheses in shrubbery notation do not disable indentation, unlike
some indentation-sensitive notations. That choice supports a language in
shrubbery notation where parentheses can be added around any expression
— even if the expression is written with indentation (although the
expression may need to be shifted right to preserve relative
indentation, depending on how parentheses are added).

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
@litchar{|} already provides one consistency check. Allowing an optional
@litchar{:} before @litchar{|} would work, but programmers may then
choose differently on omitting or including the @litchar{:}, leading to
subtly divergent conventions.

Explicit block grouping via @litchar{«} and @litchar{»} is expected to
be rare. The grouping characters were intentionally chosen from the
Latin-1 extension of ASCII to avoid reserving additional ASCII
characters.

Making whitespace and comment lines ignored in all contexts means that
they can be freely added without intefering with grouping. The
@litchar{\} continuation operator is somewhat unusual in that it skips
blank and comment lines to continue, as opposed to requiring @litchar{\}
on every continuing line; that, too, allows extra blank and comment
lines to be added, even amid continuing lines.

The interaction of indentation and @litchar{\} differs slightly from
Python, which does not count the space for @litchar{\} itself or any
leading whitespace on a continuing line toward indentation. Counting the
leading whitespace on a continuing line has the advantage that it can
reach an arbitrary amount of identation within a constrained textual
width. Counting the @litchar{\} itself is consistent with ignoring
@litchar{\} when it appears within a line, so grouping stays the same
whether there's a newline or the continue line immediately after
@litchar{\}. The whitespace role of @litchar{\} also means that spaces
can be turned into @litchar{\} to “harden” code for transfer via media
(such as email) that might mangle consecutive spaces.

Using @litchar{~} for keywords has a precedent in OCaml. Using
@litchar{~} for keywords uses up a character that might otherwise be
used for operators, but keywords seem useful enough to be worth this
cost. The notion of keywords as distinct from identifiers has been
liberating for Racket syntax (particularly since keywords can be kept
disintinct from expressions more generally), and we expect similar
benefits for having keywords in shrubbery notation.

The @litchar{#{....}} escape to S-expressions bridges between shrubbery
notation and Racket identifiers. For example,
@litchar{#{exact-integer?}} is an identifier with @litchar{-} and
@litchar{?} as part of the identifier. Shrubbery notation could be
adapted to support Lisp-style identifiers by requiring more space around
operators, but the rule for continuing a group between @litchar{(} and
@litchar{)} or @litchar{[} and @litchar{]} currently depends on
distinguishing operators from non-operators.

For @litchar["@"], the choice of treating @litchar|{@f[arg]{text}}| as
@litchar{f(arg, ["text"])} instead of @litchar{f(arg, "text")} reflects
experience with S-expression @litchar["@"] notation. Although it seems
convenient that, say @litchar|{@bold{x}}| is treated as @litchar{(bold "x")},
the consequence is that a function like @litchar{bold} might be
implemented at first to take a single argument; later, a use like
@litchar|{@bold{Hello @name}}| breaks, because two arguments are
provided. Making explicit the list that's inherent in body parsing
should help reduce such mistakes (or bad design choices) for functions
that are meant to be used with @litchar["@"] notation.

@section{Prior Art}

Indentation-sensitive parsing and the use of @litchar{:} is obviously
informed by Python.

Sampling notation's rules relating indentation, lines, @litchar{;}, and
@litchar{:} are originally based on the
@hyperlink["https://github.com/tonyg/racket-something"]{#lang something}
reader, which also targets an underlying expander that
further groups tokens. Shrubbery notation evolved away from using
@litchar{{}} for blocks, however, because @litchar{:} was nearly always
preferred in experiments with the notation. For the very rare case that
explicit grouping is needed for a block, @litchar{«} and @litchar{»} can
be used. Freeing @litchar{{}} from use for blocks, meanwhile, allows its
use for set and map notations.

Shrubbery notation is also based on
@hyperlink["https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/lexpr/0004-lexpr.md"]{Lexprs},
particularly its use of @litchar{|}. Lexprs uses mandatory @litchar{:} and @litchar{|} tokens
as a prefix for indentation, and it absorbs an additional line after
an indented section to allow further chaining of the group. Although
@litchar{«»} can be used to form multiple subgroups within a shrubbery group,
the notation discourages that style in favor of further nesting (or,
in the case of @litchar{if}, in favor of @litchar{|} notation like other
conditionals).

Shrubbery notation is in some sense a follow-up to
@hyperlink["https://github.com/mflatt/racket2-rfcs/blob/sapling/sapling/0005-sapling.md"]{sapling notation}.
The primary difference is that shrubbery notation is
indentation-sensitive, while sapling notation is
indentation-insensitive. Indentation sensitivity and block conventions
in shrubbery notation avoid some delimiters and blank lines that are
needed in sapling notation.

More generally, shrubbery notation takes inspiration from
S-expressions and alternative S-expression notations. The idea that,
even in an S-expression-like setting, some parsing can be deferred a
later parser has many precedents, including Clojure's choice of where
to put parentheses and notations that use something like @litchar{$} to escape
to infix mode.
