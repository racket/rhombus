#lang scribble/rhombus/manual
@(import:
    "grammar.rhm" open
    "grammar-s-exp.rkt" open)

@title{Parsed Representation}

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

 @item{An element created by @litchar{''} is represented by @litchar{'quotes} consed
   onto a group-sequence list.},

 @item{A block is represented as either @litchar{'block} or @litchar{'alts} consed onto a
   group-sequence list. The representation uses @litchar{'alts} if the content
   of the block is a squence of groups started with @litchar{|}, and it's
   @litchar{'block} otherwise.},

 @item{A block created to follow @litchar{|} appears immediately in an @litchar{'alts}
   list.}

]

Note that a block can only appear immediately in a @litchar{'group} or @litchar{'alts}
list, and only at the end within a @litchar{'group} list. Note also that there is no possibility of confusion between
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
                 @rhombus[[symbol(group), $$(@nonterm{term}), ..., $$(@nonterm{tail-term})]]],
                [@nonterm{term},
                 @nonterm{atom},
                 @rhombus[[symbol(op), $$(@nonterm{symbol})]],
                 @rhombus[[symbol(parens), $$(@nonterm{group}), ...]],
                 @rhombus[[symbol(brackets), $$(@nonterm{group}), ...]],
                 @rhombus[[symbol(braces), $$(@nonterm{group}), ...]],
                 @rhombus[[symbol(quotes), $$(@nonterm{group}), ...]]],
                [@nonterm{tail-term},
                 @nonterm{term},
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

