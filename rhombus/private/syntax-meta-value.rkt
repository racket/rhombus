#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax syntax_meta_value))

(begin-for-syntax
  (define syntax_meta_value
    (case-lambda
      [(id/op)
       (define id (extract-operator id/op))
       (unless id
         (raise-argument-error 'syntax_meta_value "identifier-or-operator?" id/op))
       (syntax-local-value id)]
      [(id/op fail)
       (define id (extract-operator id/op))
       (unless id
         (raise-argument-error 'syntax_meta_value "identifier-or-operator??" id/op))
       (syntax-local-value id (if (and (procedure? fail)
                                       (procedure-arity-includes? fail 0))
                                  fail
                                  (lambda () fail)))]))
  
  (define (extract-operator v)
    (cond
      [(identifier? v) v]
      [(syntax? v)
       (syntax-parse v
         #:datum-literals (op)
         [(op id) #'id]
         [_ #f])]
      [else #f])))
