#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax add-name-prefix))

(define-for-syntax (add-name-prefix prefix id)
  (if prefix
      (datum->syntax #f
                     (string->symbol (format "~a.~a" (syntax-e prefix) (syntax-e id)))
                     id)
      id))
