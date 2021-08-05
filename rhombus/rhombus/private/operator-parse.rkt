#lang racket/base
(require syntax/parse)

(provide :operator
         :operator-or-identifier)

(define-syntax-class :operator
  (pattern ((~datum op) name)))

(define-syntax-class :operator-or-identifier
  (pattern ((~datum op) name))
  (pattern name:identifier))
