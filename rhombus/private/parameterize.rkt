#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parse.rkt")

(provide (rename-out
          [rhombus-parameterize parameterize]))

(begin-for-syntax
  (define-syntax-class :binding
    #:datum-literals (block group)
    (pattern
      (group n ... ((~and block-tag block) body ...))
      #:attr name (syntax/loc #'(n ...) (group n ...)))))

(define-syntax rhombus-parameterize
  (expression-transformer
   #'rhombus-parameterize
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block group braces)
       [(form-id (braces bindings::binding ...)
                 ((~and tag-body block) body ...)
                 . tail)
        (values
         #'(parameterize ([(rhombus-expression bindings.name)
                           (rhombus-body-at bindings.block-tag bindings.body ...)]
                          ...)
             (rhombus-body-at tag-body body ...))
         #'tail)]))))
