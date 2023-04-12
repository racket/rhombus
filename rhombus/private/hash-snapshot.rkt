#lang racket/base

(provide hash-snapshot)

(define (hash-snapshot ht)
  (cond
    [(hash-equal-always? ht) (for/hashalw ([(k v) (in-hash ht)])
                               (values k v))]
    [(hash-eq? ht) (for/hasheq ([(k v) (in-hash ht)])
                     (values k v))]
    [(hash-eqv? ht) (for/hasheqv ([(k v) (in-hash ht)])
                      (values k v))]
    [else (for/hash ([(k v) (in-hash ht)])
            (values k v))]))
