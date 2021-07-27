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

           transform-in
           transform-out
           call-as-transformer))

(struct transformer (proc))

;; a generic transformer returns a form and a tail of unused syntax

;; returns a list of definitions+expressions and a list of expressions:
(property definition-transformer transformer)

;; returns a list of declarations+definitions+expression:
(property declaration-transformer transformer)

;; All helper functions from here on expect core transformers (i.e.,
;; an accessor like `transformer-ref` has already been applied).

(define no-props (datum->syntax #f #f))

(define current-transformer-introduce (make-parameter (lambda (stx) stx)))
(define (transform-in stx)
  ((current-transformer-introduce) stx))
(define (transform-out stx)
  ((current-transformer-introduce) stx))

(define (call-as-transformer id thunk)
  (define intro (make-syntax-introducer))
  (parameterize ([current-transformer-introduce intro])
    (thunk intro
           (lambda (stx)
             (let loop ([stx stx])
               (cond
                 [(syntax? stx)
                  (syntax-track-origin (intro stx)
                                       no-props
                                       id)]
                 [(pair? stx) (cons (loop (car stx))
                                    (loop (cdr stx)))]
                 [else stx]))))))

(define (apply-transformer t id stx checker)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (lambda (in out)
     (define-values (form tail) (proc (in stx)))
     (check-transformer-result (checker (out form) proc)
                               (out tail)
                               proc))))

(define (apply-definition-transformer t id stx)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (lambda (in out)
     (define-values (forms exprs) (proc (in stx)))
     (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
     (unless (stx-list? exprs) (raise-result-error (proc-name proc) "stx-list?" exprs))
     (values (out forms) (out exprs)))))

(define (apply-declaration-transformer t id stx)
  (define proc (transformer-proc t))
  (call-as-transformer
   id
   (lambda (in out)
     (define forms (proc (in stx)))
     (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
     (out forms))))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  (unless (stx-list? tail) (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))
