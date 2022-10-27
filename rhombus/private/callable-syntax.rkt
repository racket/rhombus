#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "callable.rkt"
         "name-root.rkt")

(provide callable)

(define-simple-name-root callable
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
