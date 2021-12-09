#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/name-root
                     "srcloc.rkt")
         "dot.rkt")

(provide define-simple-name-root)

(define-syntax-rule (define-simple-name-root id content ...)
  ;; portal syntax with this shape is recognized by "name-root-ref.rkt"
  (#%require (portal id (map [content content] ...))))
