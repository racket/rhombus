#lang racket/base

(provide prop:custom-map custom-map-ref
         custom-map
         custom-map-name
         custom-map-copy
         custom-map-snapshot)

(define-values (prop:custom-map custom-map? custom-map-ref)
  (make-impersonator-property 'custom-map))

(define (custom-map name copy snapshot) (vector name copy snapshot))
(define (custom-map-name cm) (vector-ref cm 0))
(define (custom-map-copy cm) (vector-ref cm 1))
(define (custom-map-snapshot cm) (vector-ref cm 2))
