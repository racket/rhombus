#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "callable.rkt"
         "pack.rkt")

(provide wrap-class-transformer)

(define-syntax (wrap-class-transformer stx)
  (syntax-parse stx
    [(_ name g make-prefix-operator)
     #:with (~var lam (:callable no-adjustments)) #'g
     #'(make-prefix-operator #'name
                             '((default . stronger))
                             'macro
                             (let ([name lam.parsed])
                               (let ([name (lambda (tail head)
                                             (name (pack-tail
                                                    (cons head (unpack-tail tail #f #f)))))])
                                 name)))]))
