#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         (submod "equal.rkt" for-parse)
         "parens.rkt"
         "parse.rkt")

(provide (for-syntax :var-decl))

(begin-for-syntax
  (define-splicing-syntax-class :var-decl
    #:datum-literals (group)
    #:description "field identifier with optional default-value expression"
    #:attributes ([bind 1] default)
    (pattern (~and (~seq t ...)
                   (~seq bind ...+ _::equal rhs ...+))
             #:do [(check-multiple-equals #`(#,group-tag t ...))]
             #:with default #`(rhombus-expression (#,group-tag rhs ...)))
    (pattern (~seq bind ...+ (tag::block g ...))
             #:with default #'(rhombus-body-at tag g ...))))
