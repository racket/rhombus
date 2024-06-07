#lang racket/base

(provide remove-use-site-scopes)

;; syntax-local-identifier-as-binding works only on identifiers
(define (remove-use-site-scopes stx)
  (cond
    [(identifier? stx) (syntax-local-identifier-as-binding stx)]
    [(syntax->list stx)
     => (lambda (l)
          (datum->syntax (syntax-local-identifier-as-binding (datum->syntax stx 'id))
                         (map remove-use-site-scopes l)))]
    [else stx]))
