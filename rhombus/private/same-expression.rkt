#lang racket/base

(provide same-expression?)

(define (same-expression? a b)
  (cond
    [(identifier? a) (and (identifier? b)
                          (free-identifier=? a b))]
    [(syntax? a) (same-expression? (syntax-e a) b)]
    [(syntax? b) (same-expression? a (syntax-e b))]
    [(pair? a) (and (pair? b)
                    (same-expression? (car a) (car b))
                    (same-expression? (cdr a) (cdr b)))]
    [else (equal? a b)]))
