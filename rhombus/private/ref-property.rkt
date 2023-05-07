#lang racket/base

(provide prop:refable
         refable-ref
         prop:setable
         setable-ref)

(define-values (prop:refable refable? refable-ref)
  (make-struct-type-property 'refable))

(define-values (prop:setable setable? setable-ref)
  (make-struct-type-property 'setable))
