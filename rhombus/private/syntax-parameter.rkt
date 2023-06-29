#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (for-syntax syntax-parameters-key
                     current-syntax-parameters-iterator
                     syntax-parameter-update)
         define-syntax-parameter
         with-syntax-parameters)

(define-for-syntax syntax-parameters-key (gensym 'sxtparams))

(define-for-syntax (current-syntax-parameters-iterator)
  (define iter (continuation-mark-set->iterator #f
                                                (list syntax-parameters-key)))
  (let wrap ([iter iter])
    (lambda ()
      (define-values (vec next) (iter))
      (values (and vec (vector-ref vec 0))
              (wrap next)))))

(define-for-syntax (syntax-parameter-update key val frame)
  (datum->syntax #f (hash-set (syntax-e frame) (syntax-e key) val)))

(define-syntax (define-syntax-parameter stx)
  (raise-syntax-error #f "should not expand" stx))

(define-syntax (with-syntax-parameters stx)
  (syntax-parse stx
    [(_ #f expr) #'expr]
    [(_ stx-params expr)
     (cond
       [(zero? (hash-count (syntax-e #'stx-params))) #'expr]
       [else
        (define-values (no-expr opaque-expr)
          (with-continuation-mark
         syntax-parameters-key #'stx-params
         (syntax-local-expand-expression #'expr #t)))
        opaque-expr])]))
