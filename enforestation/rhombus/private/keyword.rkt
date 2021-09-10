#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt")

(provide keyword)

(define-syntax keyword
  (expression-transformer
   #'keyword
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(_ (parens (group k:keyword)) . tail)
        (values (syntax/loc stx (quote k))
                #'tail)]))))
