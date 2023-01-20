#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "space-provide.rkt"
         "repetition.rkt"
         "space.rkt"
         "name-root.rkt")

(define+provide-space repet rhombus/repet
  #:fields
  (macro))

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
