#lang racket/base

(provide hash-snapshot)

(define (hash-snapshot ht)
  (if (immutable? ht)
      ht
      (for/fold ([new-ht (cond
                           [(hash-equal-always? ht) #hashalw()]
                           [(hash-eq? ht) #hasheq()]
                           [(hash-eqv? ht) #hasheqv()]
                           [else #hash()])])
                ([(key val) (in-hash ht)])
        (hash-set new-ht key val))))
