#lang rhombus/scribble/manual
@(import:
    "grammar.rhm" open
    "grammar-s-exp.rkt" open
    "quote.rhm" open)

@title(~tag: "parsed-rep"){Parsed Representation}

The parse of a shrubbery can be represented by an S-expression, which in
Rhombus terms corresponds to symbols and other non-list values within
pair lists:

@itemlist(

 @item{A shrubbery document as a sequence of groups is represented as a
   list that starts @rhombus(multi), and the rest of the list is
   a sequence of groups.}

 @item{Each group is represented as a list that starts @rhombus(group), and
   the rest of the list is a sequence of terms.}

 @item{Atom terms are represented as ``themselves'' within a group,
   including identifiers and symbols, except that an operator is
   represented as a 2-element list that is @rhombus(op) followed by the operator name
   as a symbol.}

 @item{A group sequence is represented as a list of @rhombus(group) lists.}

 @item{A term created by @parens is represented by @rhombus(parens) consed
   onto a group-sequence list.}
   
 @item{A term created by @brackets is represented by @rhombus(brackets) consed
   onto a group-sequence list.}

 @item{A term created by @braces is represented by @rhombus(braces) consed
   onto a group-sequence list.}

 @item{A term created by @quotes is represented by @rhombus(quotes) consed
   onto a group-sequence list.}

 @item{A block is represented as @rhombus(block) consed onto a
   group-sequence list. A @rhombus(block) list appears only at the end of
   a group list or just before an @rhombus(alts) list that is at the end
   of the group list.}

 @item{A sequence of alternatives is represented as @rhombus(alts)
   consed onto a list of @rhombus(block) lists, where each
   @rhombus(block) list represents a @litchar{|} alternative. An
   @rhombus(alts) list appears only at the end of a group list.}

 @item{To support mixtures of shrubbery terms and others, 3-element
   list that starts with @rhombus(parsed) is treated like an atom. The
   list's second element is typically a keyword representing a parsing
   category, and the third element is payload data.}

)

Note that there is no possibility of confusion between
symbol atoms in the input and @rhombus(group), @rhombus(block), etc., at the start
of a list in an S-expression representation, because symbol atoms will
always appear as non-initial items in a @rhombus(group) list.

Overall, the grammar of S-expression representations is as follows:

@nested(~style: #'inset, shrubbery_s_expression_grammar)

Here's the same grammar, but expressed using Rhombus constructors:

@nested(~style: #'inset,
        BNF([@nonterm{document},
             @rhombus(PairList[#'multi, #,(@nonterm{group}), ...])],
            [@nonterm{group},
             @rhombus(PairList[#'group, #,(@nonterm{item}), ..., #,(@nonterm{item})]),
             @rhombus(PairList[#'group, #,(@nonterm{item}), ..., #,(@nonterm{block})]),
             @rhombus(PairList[#'group, #,(@nonterm{item}), ..., #,(@nonterm{alts})]),
             @rhombus(PairList[#'group, #,(@nonterm{item}), ..., #,(@nonterm{block}), #,(@nonterm{alts})])],
            [@nonterm{item},
             @nonterm{atom},
             @rhombus(PairList[#'op, #,(@nonterm{symbol})]),
             @rhombus(PairList[#'parens, #,(@nonterm{group}), ...]),
             @rhombus(PairList[#'brackets, #,(@nonterm{group}), ...]),
             @rhombus(PairList[#'braces, #,(@nonterm{group}), ...]),
             @rhombus(PairList[#'quotes, #,(@nonterm{group}), ...]),
             @rhombus(PairList[#'parsed, #,(@nonterm{any}), #,(@nonterm{any})])],
            [@nonterm{block},
             @rhombus(PairList[#'block, #,(@nonterm{group}), ...])],
            [@nonterm{alts},
             @rhombus(PairList[#'alts, #,(@nonterm{block}), ...])]))

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

