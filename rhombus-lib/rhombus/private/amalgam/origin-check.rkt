#lang racket/base
(require "to-list.rkt"
         "syntax-wrap.rkt"
         "annotation-failure.rkt"
         "pack.rkt")

(provide check-origins
         check-group-origins)

(define (check-origins who stxes-in)
  (define stxes (to-list #f stxes-in))
  (define (bad) (and who (raise-annotation-failure who stxes-in "Listable.to_list && Listof(Term)")))
  (if (and stxes (andmap syntax*? stxes))
      (for/fold ([ok? #t] [terms null] #:result (if ok? (reverse terms) (bad)))
                ([stx (in-list stxes)])
        (define t (and ok? (unpack-term/maybe stx)))
        (if t
            (values #t (cons t terms))
            (values #f null)))
      (bad)))

(define (check-group-origins who stxes-in)
  (define stxes (to-list #f stxes-in))
  (define (bad) (and who (raise-annotation-failure who stxes-in "Listable.to_list && Listof(Group)")))
  (if (and stxes (andmap syntax*? stxes))
      (for/fold ([ok? #t] [terms null] #:result (if ok? (reverse terms) (bad)))
                ([stx (in-list stxes)])
        (define t (and ok? (unpack-group stx #f #f)))
        (if t
            (values #t (cons t terms))
            (values #f null)))
      (bad)))
