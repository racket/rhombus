#lang racket/base

(provide =
         hash-code
         ground-representative)

(require "interface.rkt")

(define (= a b)
  (equal? (key a) (key b)))

(define (hash-code v)
  ;; TODO: equal-hash-code(ground representative + unique type identifier)
  (equal-hash-code v))

(define (ground-representative v)
  ;; TODO: self-compose key until it reaches a key type
  ;; TODO: memoize
  (key v))
