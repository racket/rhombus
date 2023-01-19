#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "repetition.rkt"
         "space.rkt"
         "name-root.rkt")

(provide repet)

(define-name-root repet
  #:root (space-syntax rhombus/repet)
  #:fields
  (macro))

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
