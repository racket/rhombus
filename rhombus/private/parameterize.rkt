#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "expression.rkt"
         "parse.rkt"
         "parens.rkt"
         #;(submod "equal.rkt" for-parse))

(provide (rename-out
          [rhombus-parameterize parameterize]))

(begin-for-syntax
  (define-syntax-class :binding
    #:attributes (lhs rhs)
    #:datum-literals (group)
    (pattern (group n ... (tag::block body ...))
             #:with lhs #`(#,group-tag n ...)
             #:with rhs #'(rhombus-body-at tag body ...))
    #;
    (pattern (group n::not-equal ... _::equal expr ...)
             #:with lhs #`(#,group-tag n ...)
             #:with rhs #`(rhombus-expression (#,group-tag expr ...)))))

(define-syntax rhombus-parameterize
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (_::braces binding::binding ...)
                 (tag::block body ...))
        (values
         #'(parameterize ([(rhombus-expression binding.lhs) binding.rhs]
                          ...)
             (rhombus-body-at tag body ...))
         #'())]))))
