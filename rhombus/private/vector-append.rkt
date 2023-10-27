#lang racket/base

(provide vector-append)

(define (vector-append v1 v2)
  (define v3 (make-vector (+ (vector-length v1) (vector-length v2))))
  (for ([v (in-vector v1)]
        [i (in-naturals 0)])
    (vector-set! v3 i v))
  (for ([v (in-vector v2)]
        [i (in-naturals (vector-length v1))])
    (vector-set! v3 i v))
  v3)
