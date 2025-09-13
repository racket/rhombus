#lang racket/base
(require syntax/parse/pre)

(provide :name)

(define-syntax-class :name
  #:description "name"
  #:attributes (name)
  (pattern ((~datum op) name))
  (pattern name:identifier))
