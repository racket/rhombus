#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-begin begin]))

(define-syntax rhombus-begin
  (expression-transformer
   #'rhombus-begin
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id ((~and tag block) form ...)
                 . tail)
        (values
         #'(let ()
             (rhombus-block-at tag form ...))
         #'tail)]))))
