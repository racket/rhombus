#lang racket/base
(require "to-list.rkt"
         "syntax-wrap.rkt"
         "annotation-failure.rkt"
         "pack.rkt")

(provide check-origins
         check-group-origins)

(define (do-check-origins who what stxes-in unpack)
  (define stxes (to-list #f stxes-in))
  (define (bad) (and who (raise-annotation-failure who stxes-in
                                                   (format "Listable.to_list && List.of(~a)" what))))
  (if (and stxes (andmap syntax*? stxes))
      (for/fold ([ok? #t] [terms null] #:result (if ok? (reverse terms) (bad)))
                ([stx (in-list stxes)])
        (define t (and ok? (unpack stx #f #f)))
        (if t
            (values #t (cons t terms))
            (values #f null)))
      (bad)))

(define (check-origins who stxes-in)
  (do-check-origins who "Term" stxes-in unpack-term))

(define (check-group-origins who stxes-in)
  (do-check-origins who "Group" stxes-in unpack-group))
