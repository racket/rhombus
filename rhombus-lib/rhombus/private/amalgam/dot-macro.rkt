#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "pack.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     (only-in "static-info.rkt"
                              get-empty-static-infos))
         "space-provide.rkt"
         (submod "dot.rkt" for-dot-provider)
         "macro-macro.rkt"
         "wrap-expression.rkt"
         "repetition.rkt")

(define+provide-space dot rhombus/dot
  #:fields
  (macro))

(module+ for-compose
  (provide (for-syntax wrap-dot-provider-transformer)))

(define-for-syntax provider_key #'#%dot-provider)

(define-identifier-syntax-definition-transformer macro
  rhombus/dot
  #:extra ([#:is_static get-empty-static-infos value]
           [#:is_repet get-empty-static-infos value]
           [#:tail get-syntax-static-infos pattern])
  #:report-keywords
  #'make-dot-provider-transformer)

(define-for-syntax (make-dot-provider-transformer proc provided-kws)
  (dot-provider (wrap-dot-provider-transformer proc
                                               (and (memq '#:is_repet provided-kws) #t))))

(define-for-syntax (wrap-dot-provider-transformer proc handled-repet?)
  (lambda (left dot right tail static? repetition? success-k fail-k)
    (cond
      [(and repetition? (not handled-repet?)) (fail-k)]
      [else
       (define (wrap e) (if repetition?
                            (syntax-parse #`(group #,e)
                              [rep::repetition #'rep.parsed]
                              [_
                               (log-error "oops ~s" e)
                               e]) ; leave it to the macro driver to complain
                            (wrap-expression e)))
       (call-with-values
        (lambda () (proc (pack-tail #`((parsed #,(if repetition? '#:rhombus/repet '#:rhombus/expr) #,left)
                                       #,dot
                                       #,right))
                         dot
                         static?
                         repetition?
                         (pack-tail tail)))
        (case-lambda
          [(e)
           (cond
             [e (success-k (wrap e) tail)]
             [else
              (fail-k)])]
          [(e tail)
           (if e
               (success-k (wrap e) (unpack-tail tail proc #f))
               (fail-k))]))])))
