#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "entry-point.rkt"
         "name-root.rkt")

(provide entry_point)

(define-simple-name-root entry_point
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
