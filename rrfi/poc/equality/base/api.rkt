#lang racket/base

(provide =
         hash-code)

(require "interface.rkt")

(define (= a b)
  (equal? a b))

(define (hash-code v)
  (equal-hash-code v))
