#lang racket/base
(require (for-syntax racket/base))

(provide $:
         Term
         Id
         Op
         Id_Op
         Group
         Block)

(module+ for-quasiquote
  (provide (for-syntax in-syntax-class-space)))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer 'rhombus/syntax-class)))

(define-syntax $: "only in patterns")
(define-syntax Term "predefined syntax class")
(define-syntax Id "predefined syntax class")
(define-syntax Op "predefined syntax class")
(define-syntax Id_Op "predefined syntax class")
(define-syntax Group "predefined syntax class")
(define-syntax Block "predefined syntax class")
