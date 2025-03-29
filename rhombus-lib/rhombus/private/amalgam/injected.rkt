#lang racket/base

(provide (struct-out injected)
         uninject)

(struct injected (e)
  #:mutable ; so content is not converted to syntax
  #:prefab)

(define (uninject stx)
  (define e (syntax-e stx))
  (if (injected? e)
      (datum->syntax stx (injected-e e) stx stx)
      stx))
