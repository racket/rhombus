#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "introducer.rkt"
                     "name-root.rkt"
                     "name-path-op.rkt")
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "order.rkt"
         "definition.rkt"
         "dotted-sequence-parse.rkt"
         "macro-macro.rkt"
         "static-info.rkt"
         "parens.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt")

(provide (for-space rhombus/namespace
                    operator_order)
         (for-syntax (for-space rhombus/namespace
                                operator_order_meta)))

(define+provide-space operator_order rhombus/operator_order
  #:fields
  (def
   def_set))

(begin-for-syntax
  (define-name-root operator_order_meta
    #:fields
    (space)))

(define-for-syntax space
  (space-syntax rhombus/operator_order))

(define-defn-syntax def
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        [(_ name-seq::dotted-identifier-sequence)
         #:with name::dotted-identifier #'name-seq
         (list
          (build-syntax-definition/maybe-extension
           'rhombus/operator_order #'name.name #'name.extends
           #`(make-order
              null
              'left)))]
        [(_ name-seq::dotted-identifier-sequence
            (_::block
             (~var options (:order-options 'rhombus/operator_order))))
         #:with name::dotted-identifier #'name-seq
         (list
          (build-syntax-definition/maybe-extension
           'rhombus/operator_order #'name.name #'name.extends
           #`(make-order
              #,(convert-prec #'options.prec)
              #,(convert-assc #'options.assc #'()))))]))))

(define-defn-syntax def_set
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        [(_ name-seq::dotted-identifier-sequence
            (_::block
             (group order-name-seq::dotted-identifier-sequence ...)
             ...))
         #:with name::dotted-identifier #'name-seq
         (define order-names (for/list ([order-name-seq (in-list (syntax->list #'(order-name-seq ... ...)))])
                               (syntax-parse order-name-seq
                                 [(~var name (:hier-name-seq in-name-root-space in-order-space name-path-op name-root-ref))
                                  (in-order-space #'name.name)])))
         (define flat-order-names
           (apply
            append
            (for/list ([order-name (in-list order-names)])
              (define v (syntax-local-value* order-name order-set-ref))
              (unless v
                (raise-syntax-error #f
                                    "not defined as an operator order"
                                    order-name))
              (order-set->order-names v order-name))))
         (with-syntax ([(order-name ...) flat-order-names])
           (list
            (build-syntax-definition/maybe-extension
             'rhombus/operator_order #'name.name #'name.extends
             #`(order-set
                (lambda ()
                  (list (quote-syntax order-name) ...))))))]))))
