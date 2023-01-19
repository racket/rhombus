#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "reducer.rkt"
         "space.rkt"
         "name-root.rkt")

(provide reducer)

(define-name-root reducer
  #:root (space-syntax rhombus/reducer)
  #:fields
  (macro))

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
