#lang racket/base

(provide prop:appendable
         appendable-ref)

(define-values (prop:appendable appendable? appendable-ref)
  (make-struct-type-property 'appendable))
