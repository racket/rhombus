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
         String

         (for-syntax rhombus-type
                     rhombus-typed
                     rhombus-type-property
                     rhombus-syntax-local-type
                     in-type-space)

         define-type-syntax)

(begin-for-syntax
  (struct rhombus-type (predicate))

  (struct rhombus-typed (type-stx proc)
    #:property prop:procedure (struct-field-index proc))

  (define rhombus-type-property (gensym))

  (define in-type-space (make-interned-syntax-introducer 'rhombus/type))

  (define-syntax-class :type
    (pattern id:identifier
             #:do [(define v (syntax-local-value* (in-type-space #'id) rhombus-type?))]
             #:when (rhombus-type? v)
             #:attr predicate (rhombus-type-predicate v)))

  (define (make-typed-identifier id type-stx)
    (rhombus-typed
     type-stx
     (lambda (stx)
       (define (get-id stx)
         (syntax-property (datum->syntax id
                                         (syntax-e id)
                                         stx)
                          rhombus-type-property
                          type-stx))
       (syntax-parse stx
         [_:identifier (get-id stx)]
         [(rator rand ...) (datum->syntax (quote-syntax here)
                                          (cons (get-id #'rator) #'(rand ...))
                                          stx
                                          stx)]))))

  (define (rhombus-syntax-local-type e)
    (cond
      [(syntax-property e rhombus-type-property)
       => (lambda (type-stx) type-stx)]
      [(identifier? e)
       (define v (syntax-local-value* e rhombus-typed?))
       (and (rhombus-typed? v)
            (rhombus-typed-type-stx v))]
      [else #f])))

(define-syntax ::
  (binding-infix-operator
   #'::
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op t::type . new-tail)
        #:with left::binding-form form
        (values
         (cond
           [(free-identifier=? #'left.matcher-id #'identifier-succeed)
            ;; binding an identifier instead of a general pattern,
            ;; so we can bind that name to have type information
            (binding-form
             #'left.arg-id
             #'just-check-predicate-matcher
             #'bind-typed-identifier
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

(define-syntax (bind-typed-identifier stx)
  (syntax-parse stx
    [(_ arg-id (t predicate bind-id))
     #'(define-syntax bind-id
         (make-typed-identifier #'arg-id  (quote-syntax t)))]))

(define-syntax Integer (rhombus-type #'exact-integer?))
(define-syntax String (rhombus-type #'string?))

(define-syntax (define-type-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-type-space #'id)
         rhs)]))
