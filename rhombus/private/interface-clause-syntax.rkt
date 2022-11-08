#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "interface-clause.rkt"
         "name-root.rkt")

(provide interface_clause)

(define-simple-name-root interface_clause
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
