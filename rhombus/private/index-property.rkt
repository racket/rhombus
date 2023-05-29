#lang racket/base

(provide prop:indexable
         indexable-ref
         prop:setable
         setable-ref)

(define-values (prop:indexable indexable? indexable-ref)
  (make-struct-type-property 'indexable))

(define-values (prop:setable setable? setable-ref)
  (make-struct-type-property 'setable))
