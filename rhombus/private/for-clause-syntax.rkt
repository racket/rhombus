#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "for-clause.rkt"
         "name-root.rkt")

(provide for_clause)

(define-simple-name-root for_clause
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
