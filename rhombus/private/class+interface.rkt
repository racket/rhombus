#lang racket/base
(require (for-syntax racket/base)
         "class-clause.rkt"
         "interface-clause.rkt")

(provide (for-syntax make-class+interface-clause-transformer))

(begin-for-syntax
  (struct class+interface-clause-transformer (cls int)
    #:property prop:class-clause-transformer (lambda (self) (class+interface-clause-transformer-cls self))
    #:property prop:interface-clause-transformer (lambda (self) (class+interface-clause-transformer-int self)))
  (define (make-class+interface-clause-transformer proc [int-proc proc])
    (class+interface-clause-transformer
     (class-clause-transformer proc)
     (interface-clause-transformer int-proc))))
