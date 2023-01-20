#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "static-info.rkt"
         "function-arity-key.rkt")

(provide define/arity)

(define-syntax (define/arity stx)
  (syntax-parse stx
    [(_ #:name name (id . args) . body)
     #'(begin
         (define id
           (let ([name (lambda args . body)])
             name))
         (define-static-info-syntax id (#%function-arity #,(extract-arity #'args))))]
    [(_ (id . args) . body)
     #'(begin
         (define (id . args) . body)
         (define-static-info-syntax id (#%function-arity #,(extract-arity #'args))))]))

(define-for-syntax (extract-arity args)
  (let loop ([args args] [mask 1])
    (syntax-parse args
      [() mask]
      [(_:identifier . args) (loop #'args (arithmetic-shift mask 1))]
      [([_:identifier _] . args) (bitwise-ior mask
                                              (loop #'args (arithmetic-shift mask 1)))]
      [_:identifier (bitwise-not (sub1 (arithmetic-shift mask 1)))])))
