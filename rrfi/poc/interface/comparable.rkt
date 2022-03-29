#lang racket/base

(provide gen:comparable
         comparable?
         key)

(require racket/generic)

(define-generics comparable
  (key comparable))
