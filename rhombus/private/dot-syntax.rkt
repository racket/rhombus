#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "tail.rkt")
         "name-root.rkt"
         (submod "dot.rkt" for-dot-provider)
         (for-template (submod "dot.rkt" for-dot-provider))
         "syntax.rkt"
         "parse.rkt")

(provide dot)

(define-syntax dot
  (simple-name-root provider_key
                    macro))

(define provider_key #'#%dot-provider)

(define-syntax macro
  (make-identifier-syntax-definition-transformer
   (lambda (x) x)
   #'make-dot-provider-transformer))

(define-for-syntax (make-dot-provider-transformer proc)
  (dot-provider
   (lambda (left dot right)
     (define e (proc (pack-tail #`(#,left #,dot #,right)) dot))
     #`(rhombus-expression (group #,e)))))
