#lang racket/base

(provide split-at)

(define (split-at lst idx)
  (cond
    [(eqv? idx 0)
     (values '() lst)]
    [else
     (define-values (l-lst r-lst) (split-at (cdr lst) (sub1 idx)))
     (values (cons (car lst) l-lst) r-lst)]))
