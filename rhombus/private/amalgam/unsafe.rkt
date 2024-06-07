#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "declaration.rkt")

(provide (for-space rhombus/decl
                    use_unsafe))

(define-decl-syntax use_unsafe
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) (list #`(#%declare #:unsafe))]))))
