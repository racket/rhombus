#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest/syntax-local
                     enforest/hierarchical-ref-parse
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         (submod "dot.rkt" for-dot-provider))

(provide ::

         Integer
         Number
         String)

(module+ for-struct
  (provide (for-syntax rhombus-contract
                       rhombus-contract?
                       rhombus-contract-predicate
                       in-contract-space

                       rhombus-contracted
                       in-contracted-space

                       :contract
                       :contract-seq

                       syntax-local-contracted-contract)
           
           define-contract-syntax
           define-contracted-syntax

           #%result))

(begin-for-syntax
  (struct rhombus-contract (predicate))
  (define (rhombus-contract-ref v) (and (rhombus-contract? v) v))

  (define in-contract-space (make-interned-syntax-introducer 'rhombus/contract))

  (struct rhombus-contracted (contract-stx)
    #:property prop:dot-provider (lambda (self)
                                   (define stx (rhombus-contracted-contract-stx self))
                                   (and (identifier? stx)
                                        (syntax-local-value* (in-dot-provider-space stx)
                                                             dot-provider-ref))))
  (define (contracted-ref v) (and (rhombus-contracted? v) v))

  (define in-contracted-space (make-interned-syntax-introducer 'rhombus/contracted))

  (define-syntax-class :contract-seq
    (pattern (~var ref (:hierarchical-ref-seq in-contract-space))
             #:do [(define v (syntax-local-value* (in-contract-space #'ref.name) rhombus-contract-ref))]
             #:when v
             #:attr predicate (rhombus-contract-predicate v)
             #:attr name #'ref.name
             #:attr tail #'ref.tail))

  (define-splicing-syntax-class :contract
    (pattern (~seq e ...)
             #:with c::contract-seq #'(e ...)
             #:with () #'c.tail
             #:attr name #'c.name
             #:attr predicate #'c.predicate)))

(define-syntax ::
  (binding-infix-operator
   #'::
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::contract-seq)
        #:with left::binding-form form
        (values
         (cond
           [(free-identifier=? #'left.matcher-id #'identifier-succeed)
            ;; binding an identifier instead of a general pattern,
            ;; so we can bind that name to have contract information
            (binding-form
             #'left.arg-id
             #'just-check-predicate-matcher
             #'bind-contracted-identifier
             #'(t.name
                t.predicate
                left.arg-id))]
           [else (binding-form
                  #'left.arg-id
                  #'check-predicate-matcher
                  #'bind-nothing-new
                  #'(t.name
                     t.predicate
                     left.matcher-id
                     left.binder-id
                     left.data))])
         #'t.tail)]))
   'none))

(define-syntax (check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (t predicate left-matcher-id left-binder-id left-data) IF success fail)
     #'(IF (predicate arg-id)
           (left-matcher-id
            arg-id
            left-data
            IF
            success
            fail)
           fail)]))

(define-syntax (bind-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id (t predicate left-matcher-id left-binder-id left-data))
     #'(left-binder-id arg-id left-data)]))

(define-syntax (just-check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (t predicate bind-id) IF success fail)
     #'(IF (predicate arg-id)
           success
           fail)]))

(define-syntax (bind-contracted-identifier stx)
  (syntax-parse stx
    [(_ arg-id (t predicate bind-id))
     #'(begin
         (define bind-id arg-id)
         (define-dot-provider-syntax bind-id
           (rhombus-contracted (quote-syntax t))))]))

(define-syntax Integer (rhombus-contract #'exact-integer?))
(define-syntax Number (rhombus-contract #'number?))
(define-syntax String (rhombus-contract #'string?))
(define-syntax #%result #f) ;; contract constructor, probably to be replaced by -> 

(define-syntax (define-contract-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-contract-space #'id)
         rhs)]))

(define-syntax (define-contracted-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-contracted-space #'id)
         rhs)]))

(begin-for-syntax
  (define (syntax-local-contracted-contract form)
    (cond
      [(and (identifier? form)
            (syntax-local-value* (in-contracted-space form) contracted-ref))
       => (lambda (p)
            (rhombus-contracted-contract-stx p))]
      [else #f])))
