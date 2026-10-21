#lang racket/base

(provide flatten-id-tree)

(define (flatten-id-tree t)
  (let loop ([t t]
             [lst null])
    (cond
      [(identifier? t)
       (cons t lst)]
      [else
       (for/foldr ([lst lst])
                  ([t (in-list (syntax->list t))])
         (loop t lst))])))
