#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "repetition.rkt"
         "name-root.rkt")

(provide repet)

(define-simple-name-root repet
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
