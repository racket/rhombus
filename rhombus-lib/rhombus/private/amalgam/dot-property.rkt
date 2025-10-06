#lang racket/base
(require "../version-case.rkt")

(provide prop:field-name->accessor field-name->accessor? field-name->accessor-ref
         prop:field-name->mutator field-name->mutator? field-name->mutator-ref
         curry-method)

(meta-if-version-at-least
 "8.18.0.18"
 (require racket/unsafe/struct-type-property)
 (define unsafe-make-struct-type-property/guard-calls-no-arguments make-struct-type-property))

(define-values (prop:field-name->accessor field-name->accessor? field-name->accessor-ref)
  (unsafe-make-struct-type-property/guard-calls-no-arguments
   'field-name->accessor
   (lambda (field-names+ht+method-ht info)
     (define field-names (car field-names+ht+method-ht))
     (define gen-acc (list-ref info 3))
     (define field-ht
       (for/fold ([ht (cadr field-names+ht+method-ht)]) ([name (in-list field-names)]
                                                         [i (in-naturals)])
         (hash-set ht name (make-struct-field-accessor gen-acc i name))))
     (for/fold ([ht field-ht]) ([(name proc) (in-hash (cddr field-names+ht+method-ht))])
       (hash-set ht name (lambda (obj) (curry-method proc obj)))))))

(define-values (prop:field-name->mutator field-name->mutator? field-name->mutator-ref)
  (unsafe-make-struct-type-property/guard-calls-no-arguments
   'field-name->mutator
   (lambda (field-names+ht info)
     (define field-names (car field-names+ht))
     (define gen-mut (list-ref info 4))
     (for/fold ([ht (cdr field-names+ht)]) ([name (in-list field-names)]
                                            [i (in-naturals)]
                                            #:when name)
       (hash-set ht name (make-struct-field-mutator gen-mut i name))))))

(define (curry-method proc obj)
  (define-values (req-kws allowed-kws) (procedure-keywords proc))
  (cond
    [(null? allowed-kws)
     (procedure-reduce-arity-mask (lambda args
                                    (apply proc obj args))
                                  (arithmetic-shift (procedure-arity-mask proc) -1)
                                  (object-name proc))]
    [else
     (procedure-reduce-keyword-arity-mask (make-keyword-procedure
                                           (lambda (kws kw-args . args)
                                             (keyword-apply proc kws kw-args obj args)))
                                          (arithmetic-shift (procedure-arity-mask proc) -1)
                                          req-kws
                                          allowed-kws
                                          (object-name proc))]))
