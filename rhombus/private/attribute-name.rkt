#lang racket/base

(provide compose-attr-name)

(define (compose-attr-name match-id sym id bind-counter)
  ;; when `match-id` is #f, then we're opening an inline syntax class, and
  ;; we want to avoid clashes in symbolic names (as long as they have different scopes)
  (if match-id
      (datum->syntax match-id (string->symbol (format "~a.~a" (syntax-e match-id) sym)) id)
      (datum->syntax id (string->symbol (format "inline~a.~a" bind-counter sym)) id)))
