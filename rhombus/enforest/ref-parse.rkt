#lang racket/base
(require syntax/parse)

(provide :reference)

(define-syntax-class :reference
  (pattern ((~datum op) name))
  (pattern name:identifier))
