#lang racket/base
(require (for-syntax racket/base)
         "treelist.rkt"
         "to-list.rkt"
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
         (for-syntax get-entry-point-adjustment-static-infos))

(module+ for-struct
  (provide (struct-out entry-point-adjustment)
           no-adjustments)
  (define no-adjustments
    (entry-point-adjustment '() (lambda (arity stx) stx) #f)))

(define-primitive-class entry_point_meta.Adjustment entry-point-adjustment
  #:existing
  #:transparent
  #:fields
  ([(prefix_arguments prefix-arguments) ((#%index-result #,(get-syntax-static-infos))
                                         . #,(get-treelist-static-infos))]
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
  #:guard (lambda (args-in wrap is-method? info)
            (define args (to-treelist #f args-in))
            (unless (and args (for/and ([e (in-treelist args)]) (identifier? e)))
              (raise-argument-error* 'entry_point_meta.Adjustment rhombus-realm "Listable.to_list && List.of(Identifier)" args-in))
            (unless (and (procedure? wrap) (procedure-arity-includes? wrap 2))
              (raise-argument-error* 'entry_point_meta.Adjustment rhombus-realm "Function.of_arity(2)" wrap))
            (values args wrap (and is-method? #t))))
