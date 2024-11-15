#lang racket/base

(provide maybe-as-original)

(define (maybe-as-original id orig-id)
  (if (and (identifier? orig-id)
           (syntax-original? (syntax-local-introduce orig-id)))
      (syntax-property (datum->syntax id (syntax-e id) orig-id)
                       'original-for-check-syntax #t)
      id))
