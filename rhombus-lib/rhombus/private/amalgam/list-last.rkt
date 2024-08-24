#lang racket/base

(provide list-last)

(define (list-last l)
  (if (null? (cdr l))
      (car l)
      (list-last (cdr l))))
