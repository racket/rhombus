#lang racket/base
(require syntax/stx
         "check.rkt"
         "property.rkt")

;; See "parse.rkt" for general information about transformers and
;; parsing.

(provide rhombus-transformer?
         rhombus-transformer-proc

         (property-out rhombus-expression-transformer)
         (property-out rhombus-definition-transformer)
         (property-out rhombus-declaration-transformer)
         (property-out rhombus-binding-transformer))

(module+ for-parse
  (provide apply-expression-transformer
           apply-definition-transformer
           apply-declaration-transformer
           apply-binding-transformer))

(struct rhombus-transformer (proc))

;; returns an expression and a tail of unused syntax:
(property rhombus-expression-transformer rhombus-transformer)

;; returns a list of definitions+expressions and a list of expressions:
(property rhombus-definition-transformer rhombus-transformer)

;; returns a list of declarations+definitions+expression:
(property rhombus-declaration-transformer rhombus-transformer)

;; returns an list of ids, a filter expression, and a tail of unused syntax:
(property rhombus-binding-transformer rhombus-transformer)

;; All helper functions from here on expect unwrapped transformers (i.e.,
;; an accessor like `rhombus-expression-transformer-ref` has already
;; been applied).

(define (apply-expression-transformer t stx)
  (define proc (rhombus-transformer-proc t))
  (define-values (form tail) (proc stx))
  (check-transformer-result (check-expression-result form proc)
                            tail
                            proc))

(define (apply-definition-transformer t stx)
  (define proc (rhombus-transformer-proc t))
  (define-values (forms exprs) (proc stx))
  (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
  (unless (stx-list? exprs) (raise-result-error (proc-name proc) "stx-list?" exprs))
  (values forms exprs))

(define (apply-declaration-transformer t stx)
  (define proc (rhombus-transformer-proc t))
  (define forms (proc stx))
  (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
  forms)

(define (apply-binding-transformer t stx)
  (define proc (rhombus-transformer-proc t))
  (define-values (form tail) (proc stx))
  (check-transformer-result (check-binding-result form proc)
                            tail
                            proc))
