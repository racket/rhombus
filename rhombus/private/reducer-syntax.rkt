#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "reducer.rkt"
         "name-root.rkt")

(provide reducer)

(define-simple-name-root reducer
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
