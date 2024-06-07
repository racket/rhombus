#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt"
         "function-arity.rkt")

(define-static-info-key-syntax/provide #%function-arity
  (static-info-key (lambda (a b)
                     (union-arity-summaries (list (syntax->datum a) (syntax->datum b))))
                   (lambda (a b)
                     (intersect-arity-summaries (list (syntax->datum a) (syntax->datum b))))))
