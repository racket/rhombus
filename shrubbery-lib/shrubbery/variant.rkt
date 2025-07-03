#lang racket/base

(provide make-variant
         default-variant
         variant?
         variant-allow-operator?
         variant-indented-operator-continue?)

(struct variant (allow-operator?
                 indented-operator-continue?))

(define (make-variant #:allow-operator? [allow-operator? (lambda (str) #t)]
                      #:indented-operator-continue? [indented-operator-continue? (lambda (str) #t)])
  (define who 'make-variant)
  (unless (and (procedure? allow-operator?)
               (procedure-arity-includes? allow-operator? 1))
    (raise-argument-error who "(procedure-arity-includes/c 1)" allow-operator?))
  (unless (and (procedure? indented-operator-continue?)
               (procedure-arity-includes? indented-operator-continue? 1))
    (raise-argument-error who "(procedure-arity-includes/c 1)" indented-operator-continue?))
  (variant allow-operator?
           indented-operator-continue?))

(define default-variant (make-variant))
