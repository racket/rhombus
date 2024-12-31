#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(provide (for-syntax flonum-statinfo?))

(define-static-info-key-syntax/provide #%flonum
  (static-info-key (lambda (a b) (or (syntax-e a) (syntax-e b)))
                   (lambda (a b) (and (syntax-e a) (syntax-e b)))))

(define-for-syntax (flonum-statinfo? form [repet? #f])
  (define v (syntax-local-static-info form #'#%flonum))
  (and v (syntax-e v)))
