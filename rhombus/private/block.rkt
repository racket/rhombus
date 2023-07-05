#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-block block]))

(define-syntax rhombus-block
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id ((~and tag block) form ...)
                 . tail)
        (values
         (relocate+reraw
          (respan stx)
          #'(let ()
              (rhombus-body-at tag form ...)))
         #'tail)]))))
