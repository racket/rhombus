#lang racket/base
(require "treelist.rkt"
         "mutable-treelist.rkt"
         "annotation-failure.rkt")

(provide prop:Listable Listable? Listable-ref

         listable?
         to-list
         to-treelist
         maybe-list->treelist)

(define-values (prop:Listable Listable? Listable-ref)
  (make-struct-type-property 'Listable))

(define (listable? v)
  (or (treelist? v)
      (list? v)
      (vector? v)
      (mutable-treelist? v)
      (Listable? v)))

(define (to-list who v)
  (cond
    [(treelist? v) (treelist->list v)]
    [(list? v) v]
    [(vector? v) (vector->list v)]
    [(mutable-treelist? v) (mutable-treelist->list v)]
    [(general-to-treelist who v) => treelist->list]
    [else #f]))

(define (to-treelist who v)
  (cond
    [(treelist? v) v]
    [(list? v) (list->treelist v)]
    [(vector? v) (vector->treelist v)]
    [(mutable-treelist? v) (mutable-treelist-snapshot v)]
    [else (general-to-treelist who v)]))

(define (general-to-treelist who v)
  (define methods (Listable-ref v #f))
  (cond
    [(not methods)
     (and who
          (raise-annotation-failure who v "Listable"))]
    [else
     (define lst ((vector-ref methods 0) v))
     ;; guarded by method result
     lst]))

(define (maybe-list->treelist v)
  (cond
    [(pair? v) (list->treelist v)]
    [(null? v) empty-treelist]
    [else v]))
