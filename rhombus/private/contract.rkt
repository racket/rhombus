#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest/syntax-local
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt")

(provide ::

         Integer
         Number
         String)

(module+ for-struct
  (provide (for-syntax rhombus-contract
                       rhombus-contract?
                       rhombus-contract-predicate
                       rhombus-contracted
                       rhombus-contract-property
                       rhombus-syntax-local-contract
                       in-contract-space

                       :contract)
           
           define-contract-syntax))

(begin-for-syntax
  (struct rhombus-contract (predicate))

  (struct rhombus-contracted (contract-stx proc)
    #:property prop:procedure (struct-field-index proc))

  (define rhombus-contract-property (gensym))

  (define in-contract-space (make-interned-syntax-introducer 'rhombus/contract))

  (define-syntax-class :contract
    (pattern id:identifier
             #:do [(define v (syntax-local-value* (in-contract-space #'id) rhombus-contract?))]
             #:when (rhombus-contract? v)
             #:attr predicate (rhombus-contract-predicate v)))

  (define (make-contracted-identifier id contract-stx)
    (rhombus-contracted
     contract-stx
     (lambda (stx)
       (define (get-id stx)
         (syntax-property (datum->syntax id
                                         (syntax-e id)
                                         stx)
                          rhombus-contract-property
                          contract-stx))
       (syntax-parse stx
         [_:identifier (get-id stx)]
         [(rator rand ...) (datum->syntax (quote-syntax here)
                                          (cons (get-id #'rator) #'(rand ...))
                                          stx
                                          stx)]))))

  (define (rhombus-syntax-local-contract e)
    (cond
      [(syntax-property e rhombus-contract-property)
       => (lambda (contract-stx) contract-stx)]
      [(identifier? e)
       (define v (syntax-local-value* e rhombus-contracted?))
       (and (rhombus-contracted? v)
            (rhombus-contracted-contract-stx v))]
      [else #f])))

(define-syntax ::
  (binding-infix-operator
   #'::
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op t::contract . new-tail)
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
             #'(t
                t.predicate
                left.arg-id))]
           [else (binding-form
                  #'left.arg-id
                  #'check-predicate-matcher
                  #'bind-nothing-new
                  #'(t
                     t.predicate
                     left.matcher-id
                     left.binder-id
                     left.data))])
         #'new-tail)]))
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
     #'(define-syntax bind-id
         (make-contracted-identifier #'arg-id  (quote-syntax t)))]))

(define-syntax Integer (rhombus-contract #'exact-integer?))
(define-syntax Number (rhombus-contract #'number?))
(define-syntax String (rhombus-contract #'string?))

(define-syntax (define-contract-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-contract-space #'id)
         rhs)]))
