#lang racket/base
(require syntax/parse)

(provide :operator)

(define-syntax-class :operator
  (pattern ((~datum op) name))
  (pattern name:identifier))
