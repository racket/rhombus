#lang racket/base
(require "treelist.rkt"
         "to-list.rkt"
         "dot-property.rkt"
         "annotation-failure.rkt")

(provide (struct-out entry-point-adjustment)
         no-adjustments)

(struct entry-point-adjustment (prefix-arguments wrap-body method?)
  #:property prop:field-name->accessor
  (list* null
         ;; Duplicates the table that is constructed by
         ;; `define-primitive-class` in "entry-point-adjustment-meta.rkt",
         ;; but this module is often used for-syntax, and we don't need
         ;; all of Rhombus's meta support for those uses
         (hasheq 'prefix_arguments (lambda (e) (entry-point-adjustment-prefix-arguments e))
                 'wrap_body (lambda (e) (entry-point-adjustment-wrap-body e))
                 'is_method (lambda (e) (entry-point-adjustment-method? e)))
         #hasheq())
  #:guard (lambda (args-in wrap is-method? info)
            (define who 'entry_point_meta.Adjustment)
            (define args (to-treelist #f args-in))
            (unless (and args (for/and ([e (in-treelist args)]) (identifier? e)))
              (raise-annotation-failure who args-in "Listable.to_list && List.of(Identifier)"))
            (unless (and (procedure? wrap) (procedure-arity-includes? wrap 2))
              (raise-annotation-failure who wrap "Function.of_arity(2)"))
            (values args wrap (and is-method? #t))))

(define no-adjustments
  (entry-point-adjustment '() (lambda (arity stx) stx) #f))
