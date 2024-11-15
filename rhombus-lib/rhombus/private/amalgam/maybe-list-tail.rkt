#lang racket/base

(provide maybe-list-tail)

(define (maybe-list-tail l n)
  (or (and (eqv? n 0)
           l)
      (and (pair? l)
           (maybe-list-tail (cdr l) (sub1 n)))))

