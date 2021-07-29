#lang racket/base
(require (for-syntax racket/base)
         "expression.rkt"
         "definition.rkt")

(provide (for-syntax make-expression+definition-transformer))

(begin-for-syntax
  (struct expression+definition-transformer (exp def)
    #:property prop:expression-prefix-operator (lambda (self) (expression+definition-transformer-exp self))
    #:property prop:definition-transformer (lambda (self) (expression+definition-transformer-def self)))
  (define (make-expression+definition-transformer exp def)
    (expression+definition-transformer exp def)))
