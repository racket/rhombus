#lang racket/base
(require (submod "private/amalgam.rkt" dot))

(provide dynamic-dot-ref
         dynamic-dot-set!)

(define (dynamic-dot-ref obj name)
  (unless (symbol? name)
    (raise-argument-error 'dynamic-dot-ref "Symbol" name))
  (dot-lookup-by-name obj name))

(define (dynamic-dot-set! obj name val)
  (unless (symbol? name)
    (raise-argument-error 'dynamic-dot-set! "Symbol" name))
  (dot-assign-by-name obj name val))
