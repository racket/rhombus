#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt")
         "enforest.rkt")

(module+ for-class
  (provide (for-syntax in-entry-point-space)))

(provide define-entry-point-syntax)

(begin-for-syntax
  (provide (property-out entry-point-transformer)
           :entry-point
           :entry-point-arity
           (struct-out entry-point-adjustments)
           no-adjustments)

  (property entry-point-transformer transformer (arity-extract))

  (define (check-entry-point-result form proc)
    (unless (syntax? form)
      (raise-bad-macro-result (proc-name proc) "entry point" form))
    form)

  (define (check-entry-point-arity-result form proc)
    (unless (or (not form)
                (exact-integer? form)
                (and (list? form)
                     (= 3 (length form))
                     (exact-integer? (car form))
                     (list? (cadr form))
                     (andmap keyword? (cadr form))
                     (or (not (caddr form))
                         (and (list? (caddr form))
                              (andmap keyword? (caddr form))))))
      (raise-bad-macro-result (proc-name proc) "entry point arity" form))
    (datum->syntax #f form))

  (define in-entry-point-space (make-interned-syntax-introducer/add 'rhombus/entry_point))

  (struct entry-point-adjustments (prefix-arguments wrap-body method?))
  (define no-adjustments (entry-point-adjustments '() (lambda (arity stx) stx) #f))
  
  (define-rhombus-transform
    #:syntax-class (:entry-point adjustments)
    #:desc "entry-point form"
    #:in-space in-entry-point-space
    #:transformer-ref (lambda (v)
                        (define t (entry-point-transformer-ref v))
                        (and t (transformer
                                (lambda (stx)
                                  (define new-adjustments
                                    (struct-copy entry-point-adjustments adjustments
                                                 [prefix-arguments (map transform-in
                                                                        (entry-point-adjustments-prefix-arguments adjustments))]))
                                  ((transformer-proc t) stx new-adjustments)))))
    #:check-result check-entry-point-result)
  
  (define-rhombus-transform
    #:syntax-class :entry-point-arity
    #:desc "entry-point form"
    #:in-space in-entry-point-space
    #:transformer-ref (lambda (v)
                        (define t (entry-point-transformer-ref v))
                        (and t (transformer (entry-point-transformer-arity-extract t))))
    #:check-result check-entry-point-arity-result))

(define-syntax (define-entry-point-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-entry-point-space #'name) rhs))]))
