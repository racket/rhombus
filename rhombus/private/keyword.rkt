#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "literal.rkt")

(provide keyword)

(define-syntax keyword
  (make-expression+binding-prefix-operator
   #'keyword
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes) (group k:keyword)) . tail)
        (values (syntax/loc stx (quote k))
                #'tail)]))
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes) (group k:keyword)) . tail)
        (values (binding-form #'literal-infoer
                              #'k)
                #'tail)]))))

