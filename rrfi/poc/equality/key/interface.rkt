#lang racket/base

(provide gen:comparable
         comparable?
         key)

(require racket/generic
         racket/contract/base
         (only-in racket/function identity))

(define-generics comparable
  (key comparable)
  #:defaults ; for an instance of a key type, the key is itself
  ([any/c (define key identity)]))
