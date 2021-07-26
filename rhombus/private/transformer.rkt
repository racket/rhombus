#lang racket/base
(require syntax/stx
         "check.rkt"
         "property.rkt"
         "property-out.rkt")

;; See "parse.rkt" for general information about transformers and
;; parsing.

(provide transformer?
         transformer-proc
         transformer

         (property-out definition-transformer)
         (property-out declaration-transformer)

         check-transformer-result)

(module+ for-parse
  (provide apply-transformer
           apply-definition-transformer
           apply-declaration-transformer
           make-transformer-wrap))

(struct transformer (proc))

;; a generic transformer returns a form and a tail of unused syntax

;; returns a list of definitions+expressions and a list of expressions:
(property definition-transformer transformer)

;; returns a list of declarations+definitions+expression:
(property declaration-transformer transformer)

;; All helper functions from here on expect core transformers (i.e.,
;; an accessor like `transformer-ref` has already been applied).

(define (make-transformer-wrap)
  (define intro (make-syntax-introducer))
  (values intro
          (lambda (stx)
            (if (syntax? stx) (intro stx) stx))))

(define (apply-transformer t stx checker)
  (define proc (transformer-proc t))
  (define-values (in out) (make-transformer-wrap))
  (define-values (form tail) (proc (in stx)))
  (check-transformer-result (checker (out form) proc)
                            (out tail)
                            proc))

(define (apply-definition-transformer t stx)
  (define proc (transformer-proc t))
  (define-values (forms exprs) (proc stx))
  (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
  (unless (stx-list? exprs) (raise-result-error (proc-name proc) "stx-list?" exprs))
  (values forms exprs))

(define (apply-declaration-transformer t stx)
  (define proc (transformer-proc t))
  (define forms (proc stx))
  (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
  forms)

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? tail) (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))
