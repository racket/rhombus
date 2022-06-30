#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "pack.rkt")
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         (submod "dot.rkt" for-dot-provider)
         "syntax.rkt"
         "parse.rkt"
         "wrap-expression.rkt")

(provide dot
         (for-syntax dot_ct))

(define-simple-name-root dot
  macro)

(begin-for-syntax
  (define-simple-name-root dot_ct
    provider_key))

(define-for-syntax provider_key #'#%dot-provider)

(define-identifier-syntax-definition-transformer macro
  (lambda (x) x)
  #'make-dot-provider-transformer)

(define-for-syntax (make-dot-provider-transformer proc)
  (dot-provider
   (lambda (left dot right)
     (define e (proc (pack-tail #`((parsed #,left) #,dot #,right)) dot))
     (wrap-expression e))))
