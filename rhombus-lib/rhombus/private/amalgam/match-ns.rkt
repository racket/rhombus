#lang racket/base
(require "name-root.rkt"
         (submod "quasiquote.rkt" for-match-ns)
         (submod "unquote-binding-primitive.rkt" for-match-ns))

(provide (for-space rhombus/namespace
                    match))

(define-name-root match
  #:fields
  [cut
   delimit
   commit])
