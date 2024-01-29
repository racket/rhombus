#lang racket/base
(require "treelist.rkt"
         (submod "list.rkt" for-listable))

(provide listable?
         to-list
         to-treelist
         maybe-list->treelist)

(define (maybe-list->treelist v)
  (cond
    [(pair? v) (list->treelist v)]
    [(null? v) empty-treelist]
    [else v]))
