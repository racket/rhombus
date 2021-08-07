#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest/syntax-local
                     enforest/name-ref-parse
                     enforest/property
                     "srcloc.rkt"
                     "name-path-op.rkt")
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "static-info.rkt")

(provide ::

         Integer
         Number
         String)

(module+ for-struct
  (begin-for-syntax
   (provide (property-out contract)
            contract-predicate-stx
            contract-static-infos

            in-contract-space
            
            :contract
            :contract-seq))

  (provide define-contract-syntax))

(begin-for-syntax
  (property contract (predicate-stx static-infos))

  (define in-contract-space (make-interned-syntax-introducer 'rhombus/contract))

  (define-syntax-class :contract-seq
    (pattern (~var ref (:name-ref-seq in-contract-space name-path-op))
             #:do [(define v (syntax-local-value* (in-contract-space #'ref.name) contract-ref))]
             #:when v
             #:attr predicate (contract-predicate-stx v)
             #:attr static-infos (contract-static-infos v)
             #:attr name #'ref.name
             #:attr tail #'ref.tail))

  (define-splicing-syntax-class :contract
    (pattern (~seq e ...)
             #:with c::contract-seq #'(e ...)
             #:with () #'c.tail
             #:attr name #'c.name
             #:attr predicate #'c.predicate
             #:attr static-infos #'c.static-infos)))

(define-syntax ::
  (make-expression+binding-infix-operator
   #'::
   '((default . weaker))
   'macro
   'none
   ;; expression
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::contract-seq)
        (values
         (wrap-static-info*
          #`(let ([val #,form])
              (if (t.predicate val)
                  val
                  (raise-contract-failure val 't.name)))
          #'t.static-infos)
         #'t.tail)]))
   ;; binding
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
             #'(t.predicate
                t.static-infos
                left.arg-id))]
           [else (binding-form
                  #'left.arg-id
                  #'check-predicate-matcher
                  #'bind-nothing-new
                  #'(t.predicate
                     left.matcher-id
                     left.binder-id
                     left.data))])
         #'t.tail)]))))

(define-syntax (check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-binder-id left-data) IF success fail)
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
    [(_ arg-id (predicate left-matcher-id left-binder-id left-data))
     #'(left-binder-id arg-id left-data)]))

(define-syntax (just-check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (predicate static-infos bind-id) IF success fail)
     #'(IF (predicate arg-id)
           success
           fail)]))

(define-syntax (bind-contracted-identifier stx)
  (syntax-parse stx
    [(_ arg-id (predicate static-infos bind-id))
     #'(begin
         (define bind-id arg-id)
         (define-static-info-syntax/maybe bind-id . static-infos))]))

(define-syntax Integer (contract #'exact-integer? #'()))
(define-syntax Number (contract #'number? #'()))
(define-syntax String (contract #'string? #'()))

(define-syntax (define-contract-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-contract-space #'id)
         rhs)]))

(define (raise-contract-failure val contract)
  (error '::
         (string-append "value does not match contract\n"
                        "  argument: ~v\n"
                        "  contract: ~s")
         val
         contract))
