#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "parens.rkt"
         "parse.rkt")

(provide (for-syntax :else-clause))

(begin-for-syntax
  (define-syntax-class :else-clause
    #:description "`~else` clause"
    #:attributes (parsed)
    #:datum-literals (group)
    (pattern (_::block (group #:else (tag::block else-rhs ...)))
             #:with parsed #'(rhombus-body-at tag else-rhs ...))
    (pattern (_::block (group #:else else-rhs ...+))
             #:with parsed #`(rhombus-expression (#,group-tag else-rhs ...)))))
