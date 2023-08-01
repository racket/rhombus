#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer
                     enforest/property
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     "dot-property.rkt"
                     "realm.rkt")
         "enforest.rkt")

(module+ for-class
  (provide (for-syntax in-entry-point-space)))

(provide define-entry-point-syntax)

(begin-for-syntax
  (provide (property-out entry-point-transformer)
           :entry-point
           :entry-point-arity
           (struct-out entry_point_meta.Adjustment)
           check-entry-point-arity-result
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
      (raise-bad-macro-result (proc-name proc) #:syntax-for? #f "entry point arity" form))
    (datum->syntax #f form))

  (define in-entry-point-space (make-interned-syntax-introducer/add 'rhombus/entry_point))

  (struct entry_point_meta.Adjustment (prefix-arguments wrap-body method?)
    #:property prop:field-name->accessor
    (list* null
           (hasheq 'prefix_arguments (lambda (a) (entry_point_meta.Adjustment-prefix-arguments a))
                   'wrap_body (lambda (a) (entry_point_meta.Adjustment-wrap-body a))
                   'is_method (lambda (a) (entry_point_meta.Adjustment-method? a)))
           #hasheq())
    #:guard (lambda (args wrap is-method? info)
              (unless (and (list? args) (andmap identifier? args))
                (raise-argument-error* 'entry_point_meta.Adjustment rhombus-realm "List.of(Identifier)" args))
              (unless (and (procedure? wrap) (procedure-arity-includes? wrap 2))
                (raise-argument-error* 'entry_point_meta.Adjustment rhombus-realm "Function.of_arity(2)" wrap))
              (values args wrap (and is-method? #t))))
    
  (define no-adjustments (entry_point_meta.Adjustment '() (lambda (arity stx) stx) #f))
  
  (define-rhombus-transform
    #:syntax-class (:entry-point adjustments)
    #:desc "entry-point form"
    #:parsed-tag #:rhombus/entry_point
    #:in-space in-entry-point-space
    #:transformer-ref (lambda (v)
                        (define t (entry-point-transformer-ref v))
                        (and t (transformer
                                (lambda (stx)
                                  ((transformer-proc t) stx adjustments)))))
    #:check-result check-entry-point-result)
  
  (define-rhombus-transform
    #:syntax-class :entry-point-arity
    #:desc "entry-point form"
    #:parsed-tag #:rhombus/entry_point
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
