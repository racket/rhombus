#lang racket/base
(require syntax/parse/pre)

(provide :name
         :name/group)

(define-syntax-class :name
  #:description "name"
  #:attributes (name)
  (pattern ((~datum op) name))
  (pattern name:identifier))

(define-syntax-class :name/group
  #:description "name"
  #:attributes (name)
  #:datum-literals (group)
  (pattern (group ::name))
  (pattern ::name))
