#lang racket/base

(provide prop:custom-map custom-map-ref
         custom-map
         custom-map-name
         custom-map-get-empty
         custom-map-get-mutable-empty
         custom-map-get-weak-empty)

(define-values (prop:custom-map custom-map? custom-map-ref)
  (make-impersonator-property 'custom-map))

(define (custom-map name get-empty get-mutable-empty get-weak-empty)
  (vector name get-empty get-mutable-empty get-weak-empty))
(define (custom-map-name cm) (vector-ref cm 0))
(define (custom-map-get-empty cm) (vector-ref cm 1))
(define (custom-map-get-mutable-empty cm) (vector-ref cm 2))
(define (custom-map-get-weak-empty cm) (vector-ref cm 3))
