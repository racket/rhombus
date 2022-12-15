#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "tag.rkt")
         (submod "equal.rkt" for-parse)
         "parens.rkt")

(provide (for-syntax :var-decl))

(begin-for-syntax
  (define-splicing-syntax-class :var-decl
    #:datum-literals (group block)
    #:attributes ([bind 1] blk)
    (pattern (~seq bind::not-equal ...+ _::equal rhs ...+)
             #:attr blk #`(#,group-tag rhs ...))
    (pattern (~seq bind ...+ (~and rhs (_::block . _)))
             #:attr blk #'rhs)))

