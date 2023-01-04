#lang racket/base
(require syntax/parse
         "space-in.rkt")

(provide import-invert)

(define (import-invert r orig-stx orig-r)
  (define (disallow-dotted mp)
    (syntax-parse mp
      #:datum-literals (import-dotted)
      [(import-dotted . _)
       (raise-syntax-error #f "phase shifting not supported with a dotted module path" orig-stx orig-r)]
      [_ (void)]))
  (let loop ([r r])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-label for-meta
                                  rhombus-prefix-in only-spaces-in except-spaces-in)
      [((~and tag (~or rename-in only-in except-in expose-in rhombus-prefix-in only-spaces-in except-spaces-in)) imp . rest)
       (define-values (mp new-imp) (loop #'imp))
       (values mp #`(tag #,new-imp . rest))]
      [((~and tag for-label) imp)
       (define-values (mp new-imp) (loop #'imp))
       (disallow-dotted mp)
       (values mp #`(tag #,new-imp))]
      [((~and tag for-meta) phase imp)
       (define-values (mp new-imp) (loop #'imp))
       (unless (eqv? (syntax-e #'phase) 0)
         (disallow-dotted mp))
       (values mp #`(tag phase #,new-imp))]
      [_ (values r #f)])))
