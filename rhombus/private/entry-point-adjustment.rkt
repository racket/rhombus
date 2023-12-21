#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "class-primitive.rkt"
         "dot-property.rkt"
         "realm.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     entry_point_meta.Adjustment)
         (for-syntax entry-point-adjustment-static-infos))

(module+ for-struct
  (provide (struct-out entry-point-adjustment)
           no-adjustments)
  (define no-adjustments
    (entry-point-adjustment '() (lambda (arity stx) stx) #f)))

(define-primitive-class entry_point_meta.Adjustment entry-point-adjustment
  #:existing
  #:transparent
  #:fields
  ([(prefix_arguments prefix-arguments) ((#%index-result #,syntax-static-infos)
                                         . #,list-static-infos)]
   [(wrap_body wrap-body) ((#%function-arity 4))]
   [(is_method method?)])
  #:properties
  ()
  #:methods
  ())

(struct entry-point-adjustment (prefix-arguments wrap-body method?)
  #:property prop:field-name->accessor
  (list* null
         entry-point-adjustment-method-table
         #hasheq())
  #:guard (lambda (args wrap is-method? info)
            (unless (and (list? args) (andmap identifier? args))
              (raise-argument-error* 'entry_point_meta.Adjustment rhombus-realm "List.of(Identifier)" args))
            (unless (and (procedure? wrap) (procedure-arity-includes? wrap 2))
              (raise-argument-error* 'entry_point_meta.Adjustment rhombus-realm "Function.of_arity(2)" wrap))
            (values args wrap (and is-method? #t))))
