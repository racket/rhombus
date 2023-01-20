#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "space-provide.rkt"
         "entry-point.rkt"
         "space.rkt"
         "name-root.rkt")

(define+provide-space entry_point rhombus/entry_point
  #:fields
  (macro))

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
