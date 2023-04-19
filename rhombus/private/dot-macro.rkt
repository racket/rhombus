#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     (for-syntax racket/base))
         "space-provide.rkt"
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         (submod "dot.rkt" for-dot-provider)
         "macro-macro.rkt"
         "parse.rkt"
         "wrap-expression.rkt")

(define+provide-space dot rhombus/dot
  #:fields
  (macro
   macro_more_static))

(define-for-syntax provider_key #'#%dot-provider)

(define-identifier-syntax-definition-transformer macro
  rhombus/dot
  #:extra ([#:is_static (quote-syntax ())]
           [#:tail syntax-static-infos])
  #'make-dot-provider-transformer)

(define-for-syntax (make-dot-provider-transformer proc)
  (dot-provider
   (lambda (left dot right tail static? success-k fail-k)
     (call-with-values
      (lambda () (proc (pack-tail #`((parsed #,left) #,dot #,right)) dot static? tail))
      (case-lambda
        [(e)
         (cond
           [e (success-k (wrap-expression e) tail)]
           [else
            (fail-k)])]
        [(e tail)
         (if e
             (success-k (wrap-expression e) (unpack-tail tail proc #f))
             (fail-k))])))))
