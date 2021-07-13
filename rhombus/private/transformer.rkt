#lang racket/base
(require syntax/stx
         "check.rkt")

;; See "parse.rkt" for general information about transformers and
;; parsing.

(provide rhombus-expression-transformer?
         rhombus-definition-transformer?
         rhombus-declaration-transformer?
         rhombus-pattern-transformer?

         prop:rhombus-expression-transformer
         prop:rhombus-definition-transformer
         prop:rhombus-declaration-transformer
         prop:rhombus-pattern-transformer

         rhombus-expression-transformer
         rhombus-definition-transformer
         rhombus-declaration-transformer
         rhombus-pattern-transformer

         apply-expression-transformer
         apply-definition-transformer
         apply-declaration-transformer
         apply-pattern-transformer)

(struct transformer (proc))

;; returns an expression and a tail of unused syntax:
(define-values (prop:rhombus-expression-transformer
                rhombus-expression-transformer?
                rhombus-expression-transformer-ref)
  (make-struct-type-property 'rhombus-expression-transformer))
(struct expression-transformer transformer ()
  #:property prop:rhombus-expression-transformer
  (lambda (self) self))
(define (rhombus-expression-transformer proc)
  (expression-transformer proc))

;; returns a list of definitions+expressions and a list of expressions:
(define-values (prop:rhombus-definition-transformer
                rhombus-definition-transformer?
                rhombus-definition-transformer-ref)
  (make-struct-type-property 'rhombus-definition-transformer))
(struct definition-transformer transformer ()
  #:property prop:rhombus-definition-transformer
  (lambda (self) self))
(define (rhombus-definition-transformer proc)
  (definition-transformer proc))

;; returns a list of declarations+definitions+expression:
(define-values (prop:rhombus-declaration-transformer
                rhombus-declaration-transformer?
                rhombus-declaration-transformer-ref)
  (make-struct-type-property 'rhombus-declaration-transformer))
(struct declaration-transformer transformer ()
  #:property prop:rhombus-declaration-transformer
  (lambda (self) self))
(define (rhombus-declaration-transformer proc)
  (declaration-transformer proc))

;; returns an list of ids, a filter expression, and a tail of unused syntax:
(define-values (prop:rhombus-pattern-transformer
                rhombus-pattern-transformer?
                rhombus-pattern-transformer-ref)
  (make-struct-type-property 'rhombus-pattern-transformer))
(struct pattern-transformer transformer ()
  #:property prop:rhombus-pattern-transformer
  (lambda (self) self))
(define (rhombus-pattern-transformer proc)
  (pattern-transformer proc))

(define (apply-expression-transformer t stx)
  (define proc (transformer-proc ((rhombus-expression-transformer-ref t) t)))
  (define-values (form tail) (proc stx))
  (check-transformer-result (check-expression-result form proc)
                            tail
                            proc))

(define (apply-definition-transformer t stx)
  (define proc (transformer-proc ((rhombus-definition-transformer-ref t) t)))
  (define-values (forms exprs) (proc stx))
  (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
  (unless (stx-list? exprs) (raise-result-error (proc-name proc) "stx-list?" exprs))
  (values forms exprs))

(define (apply-declaration-transformer t stx)
  (define proc (transformer-proc ((rhombus-declaration-transformer-ref t) t)))
  (define forms (proc stx))
  (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
  forms)

(define (apply-pattern-transformer t stx)
  (define proc (transformer-proc ((rhombus-pattern-transformer-ref t) t)))
  (define-values (ids filter-form tail) (proc stx))
  (check-transformer-result (check-pattern-result ids filter-form proc)
                            tail
                            proc))
