#lang racket/base
(require syntax/parse/pre)

(provide :operator
         :operator-or-identifier)

(define-syntax-class :operator
  #:attributes (name)
  #:description "an operator"
  #:opaque
  (pattern ((~datum op) name)))

(define-syntax-class :operator-or-identifier
  #:attributes (name)
  (pattern o::operator
           #:attr name #'o.name)
  (pattern name:identifier))
