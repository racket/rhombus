#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "parens.rkt")

(provide (for-syntax :else-clause))

(begin-for-syntax
  (define-syntax-class :else-clause
    #:description "`~else` clause"
    #:opaque
    #:attributes (rhs)
    #:datum-literals (group)
    (pattern (_::block (group #:else (~and rhs (_::block . _)))))
    (pattern (_::block (group #:else t ...+))
             #:with rhs #`(#,group-tag t ...))))
