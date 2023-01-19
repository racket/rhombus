#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "entry-point.rkt"
         "space.rkt"
         "name-root.rkt")

(provide entry_point)

(define-name-root entry_point
  #:root (space-syntax rhombus/entry_point)
  #:fields
  (macro))

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
