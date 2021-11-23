#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax syntax_meta_value))

(begin-for-syntax
  (define syntax_meta_value
    (case-lambda
      [(id)
       (unless (identifier? id)
         (raise-argument-error 'syntax_meta_value "identifier?" id))
       (syntax-local-value id)]
      [(id fail)
       (unless (identifier? id)
         (raise-argument-error 'syntax_meta_value "identifier?" id))
       (syntax-local-value id (if (and (procedure? fail)
                                       (procedure-arity-includes? fail 0))
                                  fail
                                  (lambda () fail)))])))
