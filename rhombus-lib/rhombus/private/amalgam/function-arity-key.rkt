#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt"
         "function-arity.rkt")

(define-static-info-key-syntax/provide #%function-arity
  (static-info-key (lambda (a b)
                     (or-arity-summaries (list (syntax->datum a) (syntax->datum b))))
                   (lambda (a b)
                     (and-arity-summaries (list (syntax->datum a) (syntax->datum b))))))
