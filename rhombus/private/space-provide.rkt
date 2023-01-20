#lang racket/base
(require "provide.rkt"
         "space.rkt"
         "name-root.rkt")

(provide define+provide-space)

(define-syntax-rule (define+provide-space name space-path
                                          #:fields
                                          (field ...))
  (begin
    (provide (for-spaces (rhombus/namespace
                          rhombus/space)
                         name))
    (define-space-syntax name
      (space-syntax space-path))
    (define-name-root name
      #:fields
      (field
       ...))))
