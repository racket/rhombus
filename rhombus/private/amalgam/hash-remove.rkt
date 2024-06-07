#lang racket/base

(provide hash-remove*)

(define (hash-remove* ht ks)
  (for/fold ([ht ht]) ([k (in-list ks)])
    (hash-remove ht k)))
