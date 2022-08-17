#lang racket/base

(provide cover
         covered?)

(define (cover covered-ht sym step)
  (hash-set covered-ht sym (hash-set (hash-ref covered-ht sym #hasheqv()) step #t)))

(define (covered? covered-ht sym step)
  (hash-ref (hash-ref covered-ht sym #hasheqv()) step #f))
