#lang racket/base
(require syntax/parse/pre
         "../hier-name-parse.rkt"
         "../name-parse.rkt")

(provide :dotted-name)

(define-syntax-class (:dotted-name in-name-root-space in-space name-path-op name-root-ref)
  #:description "name"
  #:attributes (name)
  #:datum-literals (group multi)
  (pattern ::name)
  (pattern (group . (~var h (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref)))
           #:with () #'h.tail
           #:with name #'h.name)
  (pattern (multi (~var || (:dotted-name in-name-root-space in-space name-path-op name-root-ref)))))
