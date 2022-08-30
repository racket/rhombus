#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "literal.rkt")

(provide symbol)

(define-syntax symbol
  (make-expression+binding-prefix-operator
   #'symbol
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes) (group id:identifier)) . tail)
        (values (syntax/loc stx (quote id))
                #'tail)]))
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes) (group id:identifier)) . tail)
        (values (binding-form #'literal-infoer
                              #'id)
                #'tail)]))))
