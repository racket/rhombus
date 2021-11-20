#lang racket/base

(provide pack-list*
         unpack-list*)

(define (pack-list* stx depth)
  (cond
    [(eqv? depth 0) stx]
    [else (for/list ([t (in-list (syntax->list stx))])
            (pack-list* t (sub1 depth)))]))

(define (unpack-list* qs r depth)
  (datum->syntax
   qs
   (let unpack-list* ([r r] [depth depth])
     (cond
       [(eqv? depth 0)
        (when (or (null? r) (pair? r))
          (raise-arguments-error '|$| "cannot coerce list to syntax"
                                 "list" r))
        r]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack-list* r (sub1 depth))))
            (raise-argument-error '... "list?" r))]))))
