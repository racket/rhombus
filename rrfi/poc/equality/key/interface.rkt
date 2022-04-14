#lang racket/base

(provide gen:comparable
         comparable?
         key
         type-tag)

(require racket/generic
         racket/contract/base
         (only-in racket/function identity))

(define-values (prop:type-tag type-tag? type-tag)
  (make-struct-type-property 'type-tag))

(define-generics comparable
  (key comparable)
  #:defaults ; for an instance of a key type, the key is itself
  ([any/c
    (define key identity)])
  #:derive-property prop:type-tag (gensym))
