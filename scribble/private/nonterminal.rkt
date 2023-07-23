#lang racket/base
(require (for-syntax racket/base
                     rhombus/private/introducer))

(begin-for-syntax
  (provide in-nonterminal-space
           (struct-out nonterminal)
           nonterminal-ref)
  
  (define in-nonterminal-space
    (make-interned-syntax-introducer/add 'rhombus/scribble/nonterminal))
  (struct nonterminal (transformer-id))
  (define (nonterminal-ref v)
    (and (nonterminal? v) v)))
