#lang racket/base

(provide prop:printer
         printer-ref)

(define-values (prop:printer printer? printer-ref)
  (make-struct-type-property 'printer))
