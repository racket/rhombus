#lang racket/base
(require "treelist.rkt"
         (submod "list.rkt" for-listable))

(provide listable?
         to-list
         to-treelist
         maybe-list->treelist)

(define (to-list who v)
  (cond
    [(treelist? v) (treelist->list v)]
    [(list? v) v]
    [(vector? v) (vector->list v)]
    [else (treelist->list (to-treelist who v))]))

(define (maybe-list->treelist v)
  (cond
    [(pair? v) (list->treelist v)]
    [(null? v) empty-treelist]
    [else v]))
