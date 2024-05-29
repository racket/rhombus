#lang racket/base
(require (for-syntax racket/base
                     "pack.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     (only-in "static-info.rkt"
                              get-empty-static-infos))
         "space-provide.rkt"
         (submod "dot.rkt" for-dot-provider)
         "macro-macro.rkt"
         "wrap-expression.rkt")

(define+provide-space dot rhombus/dot
  #:fields
  (macro))

(module+ for-compose
  (provide (for-syntax wrap-dot-provider-transformer)))

(define-for-syntax provider_key #'#%dot-provider)

(define-identifier-syntax-definition-transformer macro
  rhombus/dot
  #:extra ([#:is_static get-empty-static-infos value]
           [#:tail get-syntax-static-infos pattern])
  #'make-dot-provider-transformer)

(define-for-syntax (make-dot-provider-transformer proc)
  (dot-provider (wrap-dot-provider-transformer proc)))

(define-for-syntax (wrap-dot-provider-transformer proc)
  (lambda (left dot right tail static? success-k fail-k)
    (call-with-values
     (lambda () (proc (pack-tail #`((parsed #:rhombus/expr #,left) #,dot #,right)) dot static? (pack-tail tail)))
     (case-lambda
       [(e)
        (cond
          [e (success-k (wrap-expression e) tail)]
          [else
           (fail-k)])]
       [(e tail)
        (if e
            (success-k (wrap-expression e) (unpack-tail tail proc #f))
            (fail-k))]))))
