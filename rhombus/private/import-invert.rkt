#lang racket/base
(require syntax/parse/pre
         "space-in.rkt")

(provide import-invert)

(define (import-invert r orig-stx orig-r)
  (let loop ([r r])
    (syntax-parse r
      #:datum-literals (rename-in only-in except-in expose-in for-label for-meta
                                  rhombus-prefix-in only-spaces-in except-spaces-in)
      [((~and tag (~or rename-in only-in except-in expose-in rhombus-prefix-in only-spaces-in except-spaces-in)) imp . rest)
       (define-values (mp new-imp) (loop #'imp))
       (values mp #`(tag #,new-imp . rest))]
      [((~and tag for-label) imp)
       (define-values (mp new-imp) (loop #'imp))
       (values mp #`(tag #,new-imp))]
      [((~and tag for-meta) phase imp)
       (define-values (mp new-imp) (loop #'imp))
       (values mp #`(tag phase #,new-imp))]
      [_ (values r #f)])))
