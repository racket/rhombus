#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt")
         "expression.rkt")

(provide (rename-out [rhombus... ...]))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of a binding or constructor"
                            #'op.name)]))))
