#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (for-syntax syntax-parameters-key
                     current-syntax-parameters-iterator
                     syntax-parameter-update
                     syntax-parameter-merge)
         define-syntax-parameter
         with-syntax-parameters)

(define-for-syntax syntax-parameters-key (gensym 'syntax-parameters))

(define-for-syntax (current-syntax-parameters-iterator)
  (define iter (continuation-mark-set->iterator #f
                                                (list syntax-parameters-key)))
  (let wrap ([iter iter])
    (lambda ()
      (define-values (vec next) (iter))
      (values (and vec (vector-ref vec 0))
              (wrap next)))))

(define-for-syntax (syntax-parameter-update key val frame)
  (datum->syntax #f (hash-set (syntax-e frame) (if (syntax? key) (syntax-e key) key) val)))

(define-for-syntax (syntax-parameter-merge new-frame frame)
  (for/fold ([frame frame]) ([(key val) (in-hash (syntax-e new-frame))])
    (syntax-parameter-update key val frame)))

(define-syntax (define-syntax-parameter stx)
  (raise-syntax-error #f "should not expand" stx))

;; also handled in splicing mode in "forwarding-sequence.rkt"
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
