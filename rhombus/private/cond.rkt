#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-if if]))

(define-syntax rhombus-if
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id test ... ((~datum alts)
                           ((~datum block) thn ...)
                           ((~datum block) els ...))
                 . tail)
        (values
         #'(if (rhombus-expression (group test ...))
               (rhombus-block thn ...)
               (rhombus-block els ...))
         #'tail)]))))
