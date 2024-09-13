#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf
    meta_label:
      scribble/bnf open)

@title(~style: [#'toc], ~tag: "bnf"){Backus-Naur Form (BNF) Grammars}

@docmodule(scribble/bnf)

The @rhombusmodname(scribble/bnf) library provides functions for
typesetting grammars in traditional BNF notation, as opposed to
shrubbery grammars.

@doc(
  ~include scribble/bnf:
    grammar
  annot.macro 'BNFEntry'
){

 Produces a table for a BNF grammar. Each argument to @rhombus(grammar)
 defines a non-terminal @rhombus(nt) with one or more productions
 @rhombus(prod0) plus @rhombus(prod)s.

 A @rhombus(BNFEntry, ~annot) for either the left-hand or right-hand of
 a production is either a @rhombus(FlowBlock, ~annot) or
 @rhombus(Content, ~annot).

For example,

@rhombusblock(
  @(import scribble/bnf)

  @(
    block:
      def open: @litchar{(}
      def close: @litchar{)}
      bnf.grammar([@bnf.nt{expr}, [@bnf.nt{id},                                  
                                   @bnf.seq(open, @bnf.plus(@bnf.nt{expr}), close),
                                   bnf.seq(open, @litchar{lambda},
                                           open, @bnf.star(@bnf.nt{id}), close,
                                           @bnf.nt{expr}, close),
                                   @bnf.nt{val}]],
                  [@bnf.nt{val}, [@bnf.alt(@bnf.nt{number}, @bnf.nt{primop})]],
                  [@bnf.nt{id}, [@elem{any name except for @litchar{lambda}}]])
  )
)

renders as

@(
  block:
    def open: @litchar{(}
    def close: @litchar{)}
    bnf.grammar([@bnf.nt{expr}, [@bnf.nt{id},                                  
                                 @bnf.seq(open, @bnf.plus(@bnf.nt{expr}), close),
                                 bnf.seq(open, @litchar{lambda},
                                         open, @bnf.star(@bnf.nt{id}), close,
                                         @bnf.nt{expr}, close),
                                 @bnf.nt{val}]],
                [@bnf.nt{val}, [@bnf.alt(@bnf.nt{number}, @bnf.nt{primop})]],
                [@bnf.nt{id}, [@elem{any name except for @litchar{lambda}}]])
)

}

@doc(
  ~include scribble/bnf:
    nt
){

 A BNF nonterminal type in angle brackets.

}

@doc(
  ~include scribble/bnf:
    seq
    alt
    group
    opt
    star
    plus
    range
){

 Functions for constructing single-line produce right-hand sides:

@itemlist(
   @item{sequencing typeset as juxtaposition}
   @item{alternatives typeset with @litchar{|} separators}
   @item{a group typeset with parentheses}
   @item{a zero-or-one choice typeset in square brackets}
   @item{a zero-or-more choice typeset with a @litchar{*} suffix}
   @item{a one-or-more choice typeset with a @litchar{+} suffix}
   @item{a n-to-m choice typeset with a @litchar{{}} suffix}
)

}

@doc(
  ~include scribble/bnf:
    seq_lines
){

 Typesets a sequence broken across multiple lines.

}

@doc(
  def etc :: Element
){

  An element to use for omitted productions or content, renders as @litchar{...}.

}
