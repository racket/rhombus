#lang racket/base
(require "mutability.rkt"
         "same-hash.rkt")

(provide hash-snapshot)

(define (hash-snapshot ht)
  (if (immutable-hash? ht)
      ht
      (for/fold ([new-ht (same-hash-empty ht)])
                ([(key val) (in-hash ht)])
        (hash-set new-ht key val))))
