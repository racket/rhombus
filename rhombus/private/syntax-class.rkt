#lang racket/base
(require (for-syntax racket/base))

(provide $:
         Term
         Group)

(module+ for-quasiquote
  (provide (for-syntax in-syntax-class-space)))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer 'rhombus/syntax-class)))

(define-syntax $: "only in patterns")
(define-syntax Term "predefined syntax class")
(define-syntax Group "predefined syntax class")
