#lang racket/base

(provide prop:contains
         contains-ref)

(define-values (prop:contains has-contains? contains-ref)
  (make-struct-type-property 'contains))
