#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "reducer.rkt"
         "space.rkt"
         "name-root.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/space)
                     reducer))

(define-space-syntax reducer
  (space-syntax rhombus/reducer))

(define-name-root reducer
  #:fields
  (macro))

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
