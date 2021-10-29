#lang racket/base

(provide pack-list*
         unpack-list*)

(define (pack-list* stx depth)
  (cond
    [(eqv? depth 0) stx]
    [else (for/list ([t (in-list (syntax->list stx))])
            (pack-list* t (sub1 depth)))]))

(define (unpack-list* r depth)
  (cond
    [(eqv? depth 0) r]
    [else
     (if (list? r)
         (datum->syntax
          #f
          (for/list ([r (in-list r)])
            (unpack-list* r (sub1 depth))))
         (raise-argument-error '... "list?" r))]))
