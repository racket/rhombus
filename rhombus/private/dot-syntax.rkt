#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "tail.rkt")
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         (submod "dot.rkt" for-dot-provider)
         "syntax.rkt"
         "parse.rkt")

(provide dot
         (for-syntax dot_ct))

(define-syntax dot
  (simple-name-root macro))

(begin-for-syntax
  (define-syntax dot_ct
    (simple-name-root provider_key)))

(define-for-syntax provider_key #'#%dot-provider)

(define-syntax macro
  (make-identifier-syntax-definition-transformer
   (lambda (x) x)
   #'make-dot-provider-transformer))

(define-for-syntax (make-dot-provider-transformer proc)
  (dot-provider
   (lambda (left dot right)
     (define e (proc (pack-tail #`((parsed #,left) #,dot #,right)) dot))
     #`(rhombus-expression (group #,e)))))
