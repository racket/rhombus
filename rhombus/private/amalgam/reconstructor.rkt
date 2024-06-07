#lang racket/base

(provide prop:reconstructor
         reconstructor-ref)

(define-values (prop:reconstructor reconstructor? reconstructor-ref)
  (make-struct-type-property 'reconstructor))
