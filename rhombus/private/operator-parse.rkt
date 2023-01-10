#lang racket/base
(require syntax/parse/pre)

(provide :operator
         :operator-or-identifier)

(define-syntax-class :operator
  #:description "an operator"
  (pattern ((~datum op) name)))

(define-syntax-class :operator-or-identifier
  #:description "an identifier or operator"
  (pattern ((~datum op) name))
  (pattern name:identifier))
