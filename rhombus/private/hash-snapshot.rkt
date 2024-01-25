#lang racket/base
(require "mutability.rkt")

(provide hash-snapshot)

(define (hash-snapshot ht)
  (if (immutable-hash? ht)
      ht
      (for/fold ([new-ht (cond
                           [(hash-equal-always? ht) #hashalw()]
                           [(hash-eq? ht) #hasheq()]
                           [(hash-eqv? ht) #hasheqv()]
                           [else #hash()])])
                ([(key val) (in-hash ht)])
        (hash-set new-ht key val))))
