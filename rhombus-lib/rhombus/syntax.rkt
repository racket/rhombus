#lang racket/base
(require "private/pack.rkt")

(provide extract-group
         extract-term
         extract-multi

         s-exp->expr-group
         s-exp->defn-group
         s-exp->decl-group)

;; result is #f or a syntax object
(define (extract-group stx)
  (unpack-group stx #f #f))

;; result is #f or a syntax object that starts `group`
(define (extract-term stx)
  (unpack-term stx #f #f))

;; result is #f or a list of syntax objects that each that start `group`
(define (extract-multi stx)
  (unpack-multi stx #f #f))

(define (s-exp->expr-group stx)
  #`(group (parsed #:rhombus/expr #,stx)))

(define (s-exp->defn-group stx)
  #`(group (parsed #:rhombus/defn #,stx)))

(define (s-exp->decl-group stx)
  #`(group (parsed #:rhombus/decl #,stx)))
