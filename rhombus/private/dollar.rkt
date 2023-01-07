#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt")

(provide $ $$)

(define-syntax $
  (expression-prefix-operator
   (quote-syntax $)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
                            "misuse outside of a pattern or template"
                            #'op)]))))

(define-syntax $$
  (expression-prefix-operator
   (quote-syntax $$)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
                            "misuse outside of a template"
                            #'op)]))))
