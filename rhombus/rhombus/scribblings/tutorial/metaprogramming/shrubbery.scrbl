#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf)

@(fun annote(content, note):
    [content,
     @elem(~style: style(#false, PairList[color_property(PairList[127,127,127])]),
           [hspace(4), "---", " ", italic(note)])])

@(fun comma_sep(content):
    annote(content, @elem{comma-separated groups}))
@(fun newline_sep(content):
    annote(content, @elem{newline-separated groups}))

@title(~tag: "shrubbery"){Shrubbery Notation}

Now that you've tried working with some Rhombus code, you probably have
lots of questions about syntax---such as where newlines and indentation
are required and how matching works inside @quotes.

@section{S-Expressions as Inspiration}

Rhombus syntax is like Lisp syntax in that it's @deftech{bicameral}:
there are two layers to the specification, and both layers must be
satisfied to write a valid expression. In Lisp, the first layer is
@deftech{S-expression} notation; the second layer is the grammar of definitions,
conditionals, function calls, etc. That second layer is expressed in
terms of the first layer.
Rhombus is similarly layered, where the first
layer is @deftech{shrubbery} notation.

S-expression notation has an especially simple grammar:
@//
@margin_note{The grammars here mingle abstract-syntax notation with
 concrete-syntax literals like @litchar{(} and @litchar{)}. The literals
 are meant to stand in place of constructor tags while suggesting the
 relevant concrete syntax.}

@nested(~style: #'inset,
        bnf.grammar(
          [@bnf.nt{term}, [@bnf.nt{atom},
                           bnf.seq(@litchar{(}, bnf.star(@bnf.nt{term}), @litchar{)})]]
        ))

We're simplifying a little bit here, as the @bnf.nt{atom} nonterminal hides a lot of complexity and
we're ignoring the distinction between pairs and (improper) lists.

Still, this abstract syntax is simple, and the concrete version mimics
the abstract version's simplicity: a @bnf.nt{term}
sequence is combined into a single using @litchar{(} and @litchar{)}
around the sequence, and whitespace appears between the
@bnf.nt{term}s in the sequence. This simplicity is a great strength
of the language design, but it also extracts a price from programmers,
requiring them to write lots of parentheses.

The value of this simple, underlying S-expression layer in the syntax
is that it lets us factor the definition of the syntax of the language.
This first layer's parsed form serves itself as the concrete syntax for the second
layer. The second layer parses into an AST, i.e., @deftech{abstract syntax} as we
normally mean it, that a compiler or interpreter can understand. The
advantage of this split for a language like Lisp or Rhombus is in the way
that the S-expression or shrubbery notation layer is defined
once and for all, applying both inside and outside of quoted forms.
Then, we can build a second, extensible layer on top; because the
first S-expression or shrubbery layer is fixed, the macro writer needs
to consider only forms that have the first layer's basic structure.

@section{Shrubbery Goals}

Shrubbery notation is design to look conventional and lightweight,
particularly relative to S-expressions. This goal is realized in two
ways:

@itemlist(

 @item{Shrubbery notation is provides most tree
 structure, but it leaves some tree structure to a second layer of
 parsing, as in @rhombus(1 + x * 3).

 Individual elements like @rhombus(1), @rhombus(+), and @rhombus(x) are
 called @deftech{terms}. A sequence of @tech{terms} where extra parsing
 will be needed is called a @deftech{group}. Groups, meanwhile, are
 typically separated by newlines or commas.

 Forms written with @parens, @brackets, @braces, or @quotes are also
 @tech{terms} on the outside, but they contain multiple @tech{groups} on the
 inside. For example, @rhombus(f(1 + x)) is a group of two terms, where
 the first term is @rhombus(f) and the second term is @rhombus((1 + x)).
 The term @rhombus((1 + x)) contains a single group that has three terms.
 Similarly, @rhombus(f(3 * x, 4 * y)) is also two terms, but the group that
 is surrounded by parentheses contains two groups, @rhombus(3 * x) and @rhombus(4 * y),
 and those two groups each consist of three terms (a number, the asterisk, and a variable).}

 @item{Large-scale block structure relies on newlines and indentation,
 instead of relying on closing @litchar{)}s or @litchar("}")s that look
 noisy and redundant in code that is otherwise formatted in a reasonable
 way.

 For example, the end of a function body is determined by outdenting.

 @rhombusblock(
   fun f(x):
     println(x)
     x + 1

   f(0)
 )

 A @deftech{block}, like the function body above, contains a sequence of
 @tech{group}s. Roughly, a @tech{group} spans one line of text within a
 block, but there are various ways for a group to span multiple
 lines. For example, an opening @litchar{(} to create a @tech{term}
 within a group might be closed with @litchar{)} on a later line.

 @rhombusblock(
   fun f(x):
     println(
       x
     ) // of the first group in the function body's block
     x + 1
 )}

)

@section{Shrubbery Abstract Syntax}

Many previous attempts to make the abstract structure of S-expressions
look less noisy also use newlines and whitespace. Shrubbery notation
differs from previous efforts by intentionally adding complexity in the
abstract structure. Shrubbery notation enables a second layer of syntax to make distinctions
among forms that used @parens, @brackets, @braces, @litchar{:} or @litchar{|},
instead making distinctions only based on a leading identifier.
Still, the shrubbery abstract
grammar is still far simpler than the AST of any realistic
programming language.

Here's the abstract syntax that a shrubbery form describes:

@nested(~style: #'inset,
        bnf.grammar(
          [@bnf.nt{document}, [bnf.star(@bnf.nt{group})]],              
          [@bnf.nt{group}, [annote(bnf.seq(bnf.star(@bnf.nt{item}),
                                           bnf.opt(@bnf.nt{block}),
                                           bnf.opt(@bnf.nt{alts})),
                                   "must be nonempty")]],
          [@bnf.nt{term}, [bnf.alt(@bnf.nt{item},
                                   @bnf.nt{block},
                                   @bnf.nt{alts})]],
          [@bnf.nt{item}, [@bnf.nt{atom},
                           comma_sep(bnf.seq(@litchar{(}, bnf.star(@bnf.nt{group}), @litchar{)}, @hspace(0))),
                           comma_sep(bnf.seq(@litchar{[}, bnf.star(@bnf.nt{group}), @litchar{]}, @hspace(0))),
                           comma_sep(bnf.seq(@litchar("{"), bnf.star(@bnf.nt{group}), @litchar("}"), @hspace(0))),
                           newline_sep(bnf.seq(@litchar("'"), bnf.star(@bnf.nt{group}), @litchar("'"), @hspace(0)))]],
          [@bnf.nt{block}, [newline_sep(bnf.seq(@litchar{:}, bnf.star(@bnf.nt{group}), @hspace(2)))]],
          [@bnf.nt{alts}, [newline_sep(bnf.plus(bnf.group(bnf.seq(@litchar("|"), @bnf.star(@bnf.nt{group})))))]],
        ))

Why these particular textual elements?

@itemlist(

 @item{Distinguishing @parens, @brackets, and @braces has clear
 advantages, and even some S-expression dialects (e.g., Clojure) make
 that distinction.}

 @item{Quoting code is important for our goals, so we add @quotes to the
 sets of bracketing forms, which both makes it distinct and highlights
 its ``quoting'' nature.}

 @item{Using @litchar{:} plus indentation for block structure reads and
 writes well.}

 @item{Using @litchar{|} to generalize @litchar{:} for multi-block forms
 helps highlight alternatives, such the cases of an @rhombus(if),
 @rhombus(match), or @seclink("patten+template"){algebraic-datatype declaration}.}

)

Although a @bnf.nt{group} can end with both a @bnf.nt{block} and an
@bnf.nt{alts}, usually only one of those (or neither) appears in a
@bnf.nt{group}. A @tech{block} created with @litchar{:} is itself
a @bnf.nt{term} within an enclosing @bnf.nt{group}, just like
@parens, @brackets, @braces, and @quotes create @bnf.nt{term}s.
Each individual
@litchar{|} form within @bnf.nt{alts} is conceptually a @tech{block},
since it has the same shape as @bnf.nt{block}---but
a @bnf.nt{term} created with @litchar{|} encompasses a
sequence of one or more @litchar{|} blocks, not each individual
@litchar{|} block.

The @bnf.nt{atom}s of shrubbery notation are mostly straightforward,
and we'll leave them to
@seclink("token-parsing", ~doc: shrubbery_doc){the
 documentation}. Note that shrubbery distinguishes
@deftech{identifiers}, such as @litchar{x} or @litchar{to_string}, from
@deftech{operators}, such as @litchar{+} and @litchar{->}. The tokens
@litchar{:} and @litchar{|} are @emph{not} operators, and neither are
@litchar{(}, @litchar{)}, @litchar{[}, @litchar{]}, @litchar("{"),
@litchar("}"), or @litchar{'}.

@section{Shrubbery Syntax Details}

Shrubbery notation is designed to look simple and natural, but as we
all know, natural language can be surprisingly complex. For shrubbery
notation, the complexities mostly involve @bnf.nt{group} formation:
where exactly newlines are required and where they are allowed.

This section offers a compact summary the key rules, but it's still too
much for a short tutorial. Rhombus programmers are mostly expected to
infer sensible rules, anyway. We advise skipping to
@seclink("ex-shrubbery"){the exercise}.

@subsection{Separators and Terminators}

The first set of rules are about separators and closers:

@itemlist(

 @item{Spaces are used to separate @bnf.nt{item}s within a
 @bnf.nt{group}, but @bnf.nt{group}s in a @bnf.star(@bnf.nt{group})
 sequence are separated either by a comma @litchar{,} or by a newline.
 Specifically, @bnf.nt{group}s in @parens, @brackets, or @braces are
 comma-separated, while @bnf.nt{group}s other places (including in
 @quotes) are separated by newlines.

 Examples:

 @rhombusblock(
  (group 1, group 2, group 3)
  
  [group 1, group 2, group 3]
  
  {group 1, group 2, group 3}
  
  'group 1
   group 2
   group 3'
  
  : group 1
    group 2
    group 3
  
  | group 1
    group 2
    group 3  
 )}

 @item{Only some of the sequence-combining forms rely on paired
 characters to mark the start and end of the sequence. Sequences that
 start with @litchar{:} or @litchar{|} rely on a newline plus reduced
 indentation to mark the end of a sequence.

 Examples:

 @rhombusblock(
  (start sequence, sequence end)

  [start sequence, sequence end]

  {start sequence, sequence end}
  
  'start sequence
   sequence end'
  
  : start sequence: start nested sequence
                    nested sequence end
    sequence end

  | start sequence
    continue sequence | start nested sequence
                        nested sequence end
    sequence end

  : start sequence | start nested sequence
                     nested sequence end
    sequence end
)}

 @item{There is one exception to the previous rule: @litchar{|} on the
 same line as another @litchar{|} ends the preceding @litchar{|}, instead
 of requiring a newline to end the preceding @litchar{|}. This exception
 applies only if the two @litchar{|}s are inside the same @parens,
 @brackets, @braces, and @quotes.

 Examples:

 @rhombusblock(
  | one alt | second alt | third alt
  | one alt (| nested alt) | second alt
)}

)

@subsection{Required and Optional Newlines}

Some newlines are required as @bnf.nt{group} separators, but there are a few
places where newlines are optional and allowed:

@itemlist(

 @item{Extra newlines are allowed between @bnf.nt{group} in a group
 sequence, including sequences where @litchar{,} separates @bnf.nt{group}s.

  Examples:

@rhombusblock(
  (group 1, group 2,
   group 3)

  (group 1,
   
   group 2)

  'group 1
   
   group 2'
)}

 @item{A newline is allowed before the first @bnf.nt{group} in a group
 sequence, such as after a @litchar{(}, @litchar{:}, or @litchar{|}.

  Examples:

@rhombusblock(  
  (
    group 1,   
    group 2
  )

  :
    group 1
    group 2

  |
    group 1
    group 2
)
}

 @item{A newline is allowed before each @litchar{|} in an @bnf.nt{alts}.
 In fact, a newline may be needed to create a nested @bnf.nt{alts}
 without ending an enclosing @bnf.nt{alts} alternative, due to the rule
 about a @litchar{|} acting as a closer for an earlier @litchar{|} on the
 same line.

  Examples:

@rhombusblock(  
  group 1
  | alt 1 nested group 1
    alt 1 nested group 2
  | alt 2 | alt 3 nested group 1
            alt 3 nested group 2
)}

)

Note that an @bnf.nt{alts} is not just one @litchar{|} form, but a
sequence of @litchar{|} forms, each with a @bnf.nt{group} sequence. The
individual @litchar{|}s within one @bnf.nt{alt}s are also separated by
newlines. A newline is therefore potentially ambiguous as a
@bnf.nt{group} separator or a @litchar{|}-form separator; the ambiguity
is resolved by also choosing to interpret the newline as a
@litchar{|}-form separator.

@rhombusblock(
  : group 1
    | alt 1 within group 1
    | alt 2 within group 1
    group 2
)

A newline as a group separator can be written with @litchar{;}. A
@litchar{;} cannot be used between groups in @parens, @brackets, or
@braces, since newlines there are optional whitespace, not separators.
When @litchar{;} is allowed, redundant @litchar{;}s are allowed and
ignored.

@rhombusblock(
  : group 1; group 2;
    group 3
)

@subsection{Indentation Rules and Variations}

When a newline is used, either to acts as a separator or when allowed
between @litchar{,}-separated groups, indentation of the newly formed
line is constrained:

@itemlist(

 @item{The indentation of the first @bnf.nt{group} in a group sequence
 must be more indented than an enclosing @bnf.nt{group} when the new
 group sequence follows @litchar{:} or @litchar{|}. There is no constraint
 on the first @bnf.nt{group} within @parens, @brackets, @braces, or @quotes.

@rhombusblock(
  enclosing group:
      nested group (
    // not a standard layout, but allowed:
    more nested group
      )
)
}

 @item{If a @bnf.nt{group} starts on the new line, and if it is not the
 first group in its sequence, it must begin at the same column as the
 first @bnf.nt{group} in its sequence.

@rhombusblock(
  enclosing group:
      nested group 1 (
    // not a standard layout, but allowed:
    more nested group A,
    more nested group B
      )
      nested group 2
)
}

 @item{If the first @litchar{|} of an @bnf.nt{alts} starts on a new
 line, it must be indented the same as its enclosing @bnf.nt{group}. The
 first @litchar{|}s of an @bnf.nt{alts} does not have to start on a new
 line.

@rhombusblock(
  enclosing group 1
  | alt in enclosing group 1

  enclosing group 2 | alt in enclosing group 2
)
}

 @item{If a @litchar{|} that is not first in its @bnf.nt{alts} starts on
 a new line, it must line up with the first @litchar{|} in the
 @bnf.nt{alts}. A non-first @litchar{|}s does not have to start on a new
 line.

@rhombusblock(
  enclosing group
  | alt 1 | alt 2
  | alt 3

  enclosing group | alt 1 | alt 2
                  | alt 3
)
}

)

@subsection{Two More Tricks}

There's more to shrubbery notation that you could learn at your leisure,
including the @litchar{#//} @bnf.nt{group}-commenting form,
@litchar("@") notation for working with text, and @guillemets for
whitespace-insensitive mode, but there are only two more details that
are essential for practical use:

@itemlist(

 @item{You can continue a @bnf.nt{group} on another line with a
 combination of extra indentation and starting with an operator (as
 opposed to an identifier). After continuing a @bnf.nt{group} once that
 way, continue again by using the same indentation and an operator.

 Examples:

@rhombusblock(
  a + b
    + c
    + d

  obj.m1()
    .m2()
    .m3()
    + 10
)}

 @item{Nesting @quotes inside of @quotes is a problem, since the same
 character is used as an opener and closer. Sometimes, you can put the
 inner @quotes inside @parens, @brackets, or @brackets. If not, use
 @guillemet_quotes for the outside quotes, or nest @guillemet_quotes
 freely.

 Examples:

@rhombusblock(
  'a ('b c')'

  '«a 'b c'»'

  '«a '«b c»'»'
)

 The first example here is different than the latter two. The first
 example is a @quotes term that contains a @parens term, and the @parens
 term contains a @quotes term. The latter two examples are each a @quotes
 term that contains an immediate @quotes term, and the @guillemets
 disappear in the transition from concrete shrubbery syntax to abstract
 shrubbery syntax.

}

)

@section(~tag: "ex-shrubbery"){Exercise}

The @rhombuslangname(shrubbery) language permits any shrubbery form in a
module body as a sequence of @bnf.nt{group}s, and it prints the
shrubbery's abstract form using an
@seclink("parsed-rep", ~doc: shrubbery_doc){S-expression
 encoding that is described in the documentation}.

For example,

@rhombusblock(
  #,(@tt{#lang}) #,(@rhombuslangname(shrubbery))

  start:
    "hello"
    "world"
  end:
    "bye"
)

prints

@nested(~style: #'#{code-inset}){
@verbatim{
(multi
 (group start (block (group "hello") (group "world")))
 (group end (block (group "bye"))))
}
}

where @rhombus(multi, ~datum) is used to combine a top-level sequence of
@tech{groups} in a module.

Experiment with @rhombus(#,(@tt{#lang}) #,(@rhombuslangname(shrubbery))),
then reverse-engineer the following outputs by
writing a shrubbery form that produces it.@margin_note{Note that if you make
the DrRacket window narrower, the parenthesized output will contain
more newlines, making it easier to see if you got the answer right.}

@itemlist(

 @item{
   @nested(~style: #'#{code-inset}){
     @verbatim{
(multi
 (group fun
        f
        (parens (group x))
        (block (group x (op +) 1)))
 (group f
        (parens (group 2))))
     }
   }

   Solution: @local_file("shrubbery1_soln.rhm").
 }

 @item{
   @nested(~style: #'#{code-inset}){
     @verbatim{
(multi
 (group
  (quotes
   (group math
          (op |.|)
          max
          (parens (group (op $) x) (group (op ...))))
   (group (op ...)))))
     }
   }

   Solution: @local_file("shrubbery2_soln.rhm").
 }

 @item{
   @nested(~style: #'#{code-inset}){
     @verbatim{
(multi
 (group
  match
  x
  (alts
   (block (group 1 (block (group "one"))))
   (block (group 2 (block (group "two")))))))
     }
   }

   Solution: @local_file("shrubbery3_soln.rhm").
 }

)
