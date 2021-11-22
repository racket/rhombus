#lang racket/base

(provide pack-list*
         unpack-list*
         pack-group*
         unpack-group*
         unpack-single-term-group)

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
        (cond
          [(and qs (group-syntax? r))
           (define l (syntax->list r))
           (if (= 2 (length l))
               (cadr l)
               (raise-arguments-error '|$| "multi-term group syntax not allowed in term context"
                                      "group syntax" r))]
          [else r])]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack-list* r (sub1 depth))))
            (raise-argument-error '... "list?" r))]))))

(define (unpack-single-term-group r)
  (cond
    [(group-syntax? r)
     (define l (syntax->list r))
     (if (= 2 (length l))
         (cadr l)
         r)]
    [else r]))

(define (pack-group* stx depth)
  (pack-list* stx depth))

(define (unpack-group* qs r depth)
  (datum->syntax
   qs
   (let unpack-group* ([r r] [depth depth])
     (cond
       [(eqv? depth 0)
        (if (group-syntax? r)
            r
            (list 'group r))]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack-group* r (sub1 depth))))
            (raise-argument-error '... "list?" r))]))))

(define (group-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'group (syntax-e (car (syntax-e r))))))
