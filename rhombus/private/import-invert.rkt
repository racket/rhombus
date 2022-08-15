#lang racket/base
(require syntax/parse)

(provide import-invert)

(define (import-invert r)
  (syntax-parse r
    #:datum-literals (rename-in only-in except-in expose-in for-label for-meta rhombus-prefix-in)
    [((~and tag (~or rename-in only-in except-in expose-in for-label rhombus-prefix-in)) imp . rest)
     (define-values (mp new-imp) (import-invert #'imp))
     (values mp #`(tag #,new-imp . rest))]
    [((~and tag for-meta) phase imp)
     (define-values (mp new-imp) (import-invert #'imp))
     (values mp #`(tag phase #,new-imp))]
    [_ (values r #f)]))
