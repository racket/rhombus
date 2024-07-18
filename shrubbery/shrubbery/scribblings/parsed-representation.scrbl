#lang scribble/rhombus/manual
@(import:
    "grammar.rhm" open
    "grammar-s-exp.rkt" open
    "quote.rhm" open)

@title(~tag: "parsed-rep"){Parsed Representation}

The parse of a shrubbery can be represented by an S-expression:

@itemlist(

 @item{Each group is represented as a list that starts @litchar{'group}, and
   the rest of the list are the elements of the group.}

 @item{Atom elements are represented as ``themselves'' within a group,
   including identifers a symbols, except that an operator is
   represented as a 2-element list that is @litchar{'op} followed by the operator name
   as a symbol.}

 @item{A group sequence is represented as a list of @litchar{'group} lists.}

 @item{An element created by @parens is represented by @litchar{'parens} consed
   onto a group-sequence list.}
   
 @item{An element created by @brackets is represented by @litchar{'brackets} consed
   onto a group-sequence list.}

 @item{An element created by @braces is represented by @litchar{'braces} consed
   onto a group-sequence list.}

 @item{An element created by @quotes is represented by @litchar{'quotes} consed
   onto a group-sequence list.}

 @item{A block is represented as @litchar{'block} consed onto a
   group-sequence list. A @litchar{'block} list appears only at the end of
   a group list or just before an @litchar{'alts} list that is at the end
   of the group list.}

 @item{A sequence of alternatives is represented as @litchar{'alts}
   consed onto a list of @litchar{'block} lists, where each
   @litchar{'block} list represents a @litchar{|} alternative. An
   @litchar{'alts} list appears only at the end of a group list.}

)

Note that there is no possibility of confusion between
symbol atoms in the input and @litchar{'group}, @litchar{'block}, etc., at the start
of a list in an S-expression representation, because symbol atoms will
always appear as non-initial items in a @litchar{'group} list.

Overall, the grammar of S-expression representations is as follows:

@nested(~style: #'inset, shrubbery_s_expression_grammar)

Here's the same grammar, but expressed using Rhombus constructors:

@nested(~style: #'inset,
        BNF([@nonterm{document},
             @rhombus([#'top, #,(@nonterm{group}), ...])],
            [@nonterm{group},
             @rhombus([#'group, #,(@nonterm{item}), ..., #,(@nonterm{item})]),
             @rhombus([#'group, #,(@nonterm{item}), ..., #,(@nonterm{block})]),
             @rhombus([#'group, #,(@nonterm{item}), ..., #,(@nonterm{alts})]),
             @rhombus([#'group, #,(@nonterm{item}), ..., #,(@nonterm{block}), #,(@nonterm{alts})])],
            [@nonterm{item},
             @nonterm{atom},
             @rhombus([#'op, #,(@nonterm{symbol})]),
             @rhombus([#'parens, #,(@nonterm{group}), ...]),
             @rhombus([#'brackets, #,(@nonterm{group}), ...]),
             @rhombus([#'braces, #,(@nonterm{group}), ...]),
             @rhombus([#'quotes, #,(@nonterm{group}), ...])],
            [@nonterm{block},
             @rhombus([#'block, #,(@nonterm{group}), ...])],
            [@nonterm{alts},
             @rhombus([#'alts, #,(@nonterm{block}), ...])]))

Here are some example shrubberies with their S-expression parsed
representations:

@verbatim(~indent: 2){
def pi = 3.14

(group def pi (op =) 3.14)

fun fourth(n :: Int):
  let m = n*n
  let v = m*m
  println(n +& "^4 = " +& v)
  v

(group fun fourth (parens (group n (op ::) Int))
       (block
        (group let m (op =) n (op *) n)
        (group let v (op =) m (op *) m)
        (group println (parens (group n (op +&) "^4 = " (op +&) v)))
        (group v)))

if x == y
| #'same
| #'different

(group if x (op ==) y
       (alts (block (group (op |#'|) same))
             (block (group (op |#'|) different))))

fun fib(n):
  match n
  | 0: 0
  | 1: 1
  | n: fib(n-1) + fib(n-2)

(group fun fib (parens (group n))
       (block
        (group match n
               (alts
                (block (group 0 (block (group 0))))
                (block (group 1 (block (group 1))))
                (block (group n (block
                                 (group fib (parens (group n (op -) 1))
                                        (op +)
                                        fib (parens (group n (op -) 2))))))))))))
}

