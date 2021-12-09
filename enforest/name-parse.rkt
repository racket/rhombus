#lang racket/base
(require syntax/parse)

(provide :name)

(define-syntax-class :name
  (pattern ((~datum op) name))
  (pattern name:identifier))
