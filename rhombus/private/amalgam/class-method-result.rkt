#lang racket/base
(require enforest/syntax-local)

(provide (struct-out method-result)
         (struct-out method-result-maker)
         method-result-ref
         syntax-local-method-result)

(struct method-result (handler-expr predicate? count annot-str static-infos arity))

(struct method-result-maker (proc))

(define (method-result-ref v) (or (and (method-result? v) v)
                                  (and (method-result-maker? v)
                                       ((method-result-maker-proc v)))))

(define (syntax-local-method-result id)
  (or (syntax-local-value* id method-result-ref)
      (raise-syntax-error #f "could not get method result information" id)))
