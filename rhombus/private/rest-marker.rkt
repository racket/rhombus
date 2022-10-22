#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt")

(provide & ~&)

(define-syntax &
  (expression-prefix-operator
   (quote-syntax &)
   '((default . weaker))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
          "misuse outside of function parameters, a call, or a constructor"
          #'op)]))))

(define-syntax ~&
  (expression-prefix-operator
   (quote-syntax ~&)
   '((default . weaker))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
          "misuse outside of function parameters or a function call"
          #'op)]))))
