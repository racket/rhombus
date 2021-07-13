#lang racket/base
(require syntax/stx)

;; See "parse.rkt" for general information about transformers and
;; parsing.
;;
;; TODO: support transformerness as a structure-type property

(provide (struct-out rhombus-transformer)
         (struct-out rhombus-expression-transformer)
         (struct-out rhombus-definition-transformer)
         (struct-out rhombus-declaration-transformer)

         apply-expression-transformer
         apply-definition-transformer
         apply-declaration-transformer)

(struct rhombus-transformer (proc))

;; returns an expression and a tail of unused syntax:
(struct rhombus-expression-transformer rhombus-transformer ())

;; returns a list of definitions+expressions and a list of expressions:
(struct rhombus-definition-transformer rhombus-transformer ())

;; returns a list of declarations+definitions+expression:
(struct rhombus-declaration-transformer rhombus-transformer ())

(define (apply-expression-transformer t stx)
  (call-with-values
   (lambda () ((rhombus-transformer-proc t) stx))
   (case-lambda
     [(form tail)
      (unless (syntax? form) (raise-result-error 'rhombus-tranform "syntax?" form))
      (unless (stx-list? tail) (raise-result-error 'rhombus-tranform "stx-list?" tail))
      (values form tail)])))

(define (apply-definition-transformer t stx)
  (call-with-values
   (lambda () ((rhombus-transformer-proc t) stx))
   (case-lambda
     [(forms exprs)
      (unless (stx-list? forms) (raise-result-error 'rhombus-tranform-definition "stx-list?" forms))
      (unless (stx-list? exprs) (raise-result-error 'rhombus-tranform-definition "stx-list?" exprs))
      (values forms exprs)])))

(define (apply-declaration-transformer t stx)
  (call-with-values
   (lambda () ((rhombus-transformer-proc t) stx))
   (case-lambda
     [(forms)
      (unless (stx-list? forms) (raise-result-error 'rhombus-tranform-declaration "stx-list?" forms))
      forms])))
