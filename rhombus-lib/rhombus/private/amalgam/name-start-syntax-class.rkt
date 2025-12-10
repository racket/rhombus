#lang racket/base
(require syntax/parse/pre
         enforest/hier-name-parse
         "name-path-op.rkt"
         (for-template "name-root-space.rkt"
                       "name-root-ref.rkt"))

(provide :name-start)

(define-syntax-class (:name-start in-space [bound? #f])
  #:description "name or dotted name"
  #:attributes (name head tail)
  #:datum-literals (group)
  (pattern (group . g)
           #:with (~var n (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref/maybe)) #'g
           #:attr name #'n.name
           #:when (or (not bound?)
                      (let ([spaced-name (in-space #'n.name)])
                        (if (bound-identifier=? spaced-name #'n.name)
                            (identifier-binding spaced-name (syntax-local-phase-level) #t)
                            (identifier-distinct-binding spaced-name #'n.name (syntax-local-phase-level) #t))))
           #:attr head #'n.head
           #:attr tail #'n.tail))
