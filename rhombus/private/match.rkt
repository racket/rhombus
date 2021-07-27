#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "parse.rkt"
         "function.rkt")

(provide match)

(define-syntax match
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id in ... ((~and alts-tag (~datum alts))
                         ((~datum block) ((~datum group) bind ...
                                                         (~and rhs ((~datum block) . _))))
                         ...)
                 . tail)
        #:with (b::binding ...) #'((group bind ...) ...)
        (values
         #`(#,(build-case-function #'match
                                   (map list (syntax->list #'(b ...)))
                                   (map list (syntax->list #'(b.expanded ...)))
                                   (syntax->list #'(rhs ...))
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]))))
