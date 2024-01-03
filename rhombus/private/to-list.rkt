#lang racket/base
(require "treelist.rkt"
         "realm.rkt"
         (submod "list.rkt" for-listable))

(provide listable?
         to-list
         to-treelist
         maybe-list->treelist)

(define (to-list who v)
  (cond
    [(treelist? v) (treelist->list v)]
    [(list? v) v]
    [(listable? v) (treelist->list (to-treelist who v))]
    [else (raise-argument-error* who rhombus-realm "Listable" v)]))

(define (maybe-list->treelist v)
  (cond
    [(pair? v) (list->treelist v)]
    [(null? v) empty-treelist]
    [else v]))
