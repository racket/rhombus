#lang racket/base
(require "key-comp-property.rkt")

(provide same-hash?)

(define (same-hash? a b)
  (cond
    [(hash-equal-always? a) (hash-equal-always? b)]
    [(hash-eq? a) (hash-eq? b)]
    [(hash-eqv? a) (hash-eqv? b)]
    [(hash-equal? a)
     (and (hash-equal? b)
          (eq? (custom-map-ref a #f)
               (custom-map-ref b #f)))]
    [else #f]))
