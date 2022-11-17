#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "class-parse.rkt"))

(provide define-class-desc-syntax)
         
(define-syntax (define-class-desc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-desc-space #'id)
         rhs)]))

