#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     enforest/syntax-local
                     "introducer.rkt"
                     "name-root.rkt"
                     (for-syntax racket/base)))

(provide define-order-syntax
         (for-syntax in-order-space
                     order-quote
                     (rename-out [order make-order])
                     order-set
                     order-set-ref
                     order-set->order-names))

(begin-for-syntax
  (define in-order-space (make-interned-syntax-introducer/add 'rhombus/operator_order))

  (struct order-set (get-order-names))

  (define (order-set-ref v) (if (order-set? v) v (order-ref v)))

  (define (order-set->order-names v name)
    (cond
      [(order-set? v) ((order-set-get-order-names v))]
      [else (list name)]))

  (define-syntax (order-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/operator_order) #'id))])))

(define-syntax (define-order-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-order-space #'id)
         rhs)]))
