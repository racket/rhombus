#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest/syntax-local
                     enforest/hierarchical-ref-parse
                     enforest/property
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         (submod "dot.rkt" for-dot-provider))

(provide ::

         Integer
         Number
         String)

(module+ for-struct
  (begin-for-syntax
   (provide (property-out contract)
            contract-predicate-stx
            contract-dot-provider-stx

            in-contract-space

            contracted
            in-contracted-space
            indirect-dot-provider
            
            :contract
            :contract-seq
            
            syntax-local-result-dot-provider))

  (provide define-contract-syntax
           define-contracted-syntax
           
           #%result))

(begin-for-syntax
  (property contract (predicate-stx dot-provider-stx))

  (define in-contract-space (make-interned-syntax-introducer 'rhombus/contract))

  (struct contracted (result-dot-provider-stx))
  (define (contracted-ref v) (and (contracted? v) v))
  
  (struct indirect-dot-provider (dot-provider-stx)
    #:property prop:dot-provider (lambda (self)
                                   (define stx (indirect-dot-provider-dot-provider-stx self))
                                   (syntax-local-value* (in-dot-provider-space stx)
                                                        dot-provider-ref)))

  (define in-contracted-space (make-interned-syntax-introducer 'rhombus/contracted))

  (define-syntax-class :contract-seq
    (pattern (~var ref (:hierarchical-ref-seq in-contract-space))
             #:do [(define v (syntax-local-value* (in-contract-space #'ref.name) contract-ref))]
             #:when v
             #:attr predicate (contract-predicate-stx v)
             #:attr dot-provider (contract-dot-provider-stx v)
             #:attr name #'ref.name
             #:attr tail #'ref.tail))

  (define-splicing-syntax-class :contract
    (pattern (~seq e ...)
             #:with c::contract-seq #'(e ...)
             #:with () #'c.tail
             #:attr name #'c.name
             #:attr predicate #'c.predicate
             #:attr dot-provider #'c.dot-provider)))

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
         (wrap-dot-provider
          #`(let ([val #,form])
              (if (t.predicate val)
                  val
                  (raise-contract-failure val 't.name)))
          #'t.dot-provider)
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
                t.dot-provider
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
    [(_ arg-id (predicate dot-provider bind-id) IF success fail)
     #'(IF (predicate arg-id)
           success
           fail)]))

(define-syntax (bind-contracted-identifier stx)
  (syntax-parse stx
    [(_ arg-id (predicate dot-provider bind-id))
     #'(begin
         (define bind-id arg-id)
         (define-dot-provider-syntax/maybe bind-id
           (indirect-dot-provider (quote-syntax dot-provider))))]))

(define-syntax Integer (contract #'exact-integer? #'#f))
(define-syntax Number (contract #'number? #'#f))
(define-syntax String (contract #'string? #'#f))
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

(define-for-syntax (syntax-local-result-dot-provider rator)
  (cond
    [(and (identifier? rator)
          (syntax-local-value* (in-contracted-space rator) contracted-ref))
     => (lambda (cted)
          (contracted-result-dot-provider-stx cted))]
    [else #f]))

(define (raise-contract-failure val contract)
  (error '::
         (string-append "value does not match contract\n"
                        "  argument: ~v\n"
                        "  contract: ~s")
         val
         contract))
