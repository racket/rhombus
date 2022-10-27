#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "callable.rkt"
         "pack.rkt")

(provide wrap-class-binder)

(define-syntax (wrap-class-binder stx)
  (syntax-parse stx
    [(_ name core-bind-name core-name g make-binding-prefix-operator)
     #:do [(define adjustments (callable-adjustments (list #'core-name) (lambda (stx) stx) #f))]
     #:with (~var lam (:callable adjustments)) #'g
     #'(make-binding-prefix-operator #'name
                                     '((default . stronger))
                                     'macro
                                     (let ([name lam.parsed])
                                       (let ([name (lambda (tail head)
                                                     (name (quote-syntax core-bind-name)
                                                           (pack-tail
                                                            (cons head (unpack-tail tail #f #f)))))])
                                         name)))]))
