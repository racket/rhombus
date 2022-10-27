#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "class-clause.rkt"
         "name-root.rkt")

(provide class_clause)

(define-simple-name-root class_clause
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
