#lang racket/base

(provide =
         hash-code)

(define (= a b)
  (equal? a b))

(define (hash-code v)
  (equal-hash-code v))
