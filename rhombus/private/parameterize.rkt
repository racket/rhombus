#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parse.rkt"
         "parens.rkt"
         (submod "equal.rkt" for-parse))

(provide (rename-out
          [rhombus-parameterize parameterize]))

(begin-for-syntax
  (define-syntax-class :binding
    #:attributes (lhs rhs)
    #:datum-literals (group)
    (pattern (group n ... (tag::block body ...))
             #:attr lhs #'(group n ...)
             #:attr rhs #'(rhombus-body-at tag body ...))
    #;
    (pattern (group n::not-equal ... _::equal expr ...)
             #:attr lhs #'(group n ...)
             #:attr rhs #'(rhombus-expression (group expr ...)))))

(define-syntax rhombus-parameterize
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals ()
       [(form-id (_::braces binding::binding ...)
                 (tag::block body ...))
        (values
         #'(parameterize ([(rhombus-expression binding.lhs) binding.rhs]
                          ...)
             (rhombus-body-at tag body ...))
         #'())]))))
