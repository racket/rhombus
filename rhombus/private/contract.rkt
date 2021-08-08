#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest
                     enforest/operator
                     enforest/syntax-local
                     enforest/name-ref-parse
                     enforest/property
                     enforest/proc-name
                     enforest/operator
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "tail.rkt")
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "static-info.rkt")

(provide ::
         is_a

         Integer
         Number
         String)

(module+ for-struct
  (begin-for-syntax
    (provide (property-out contract-prefix-operator)
             (property-out contract-infix-operator)

             identifier-contract
             
             in-contract-space

             check-contract-result

             :contract
             :contract-form
             :inline-contract

             contract-form))
  
  (provide define-contract-syntax))

(begin-for-syntax
  (property contract-prefix-operator prefix-operator)
  (property contract-infix-operator infix-operator)

  (property contract (predicate-stx static-infos))

  (define in-contract-space (make-interned-syntax-introducer 'rhombus/contract))

  (define (raise-not-a-contract id)
    (raise-syntax-error #f
                        "not bound as a contract"
                        id))

  (define (check-contract-result stx proc)
    (unless (and (syntax? stx)
                 (let ([l (syntax->list stx)])
                   (and l
                        (= (length l) 2))))
      (raise-result-error (proc-name proc)
                          "contract-syntax?"
                          stx))
    stx)

  (define-enforest
    #:enforest enforest-contract
    #:syntax-class :contract
    #:infix-more-syntax-class :contract-infix-op+form+tail
    #:desc "contract"
    #:operator-desc "contract operator"
    #:in-space in-contract-space
    #:name-path-op name-path-op
    #:prefix-operator-ref contract-prefix-operator-ref
    #:infix-operator-ref contract-infix-operator-ref
    #:check-result check-contract-result
    #:make-identifier-form raise-not-a-contract)

  (define-syntax-class :contract-seq
    (pattern stxes
             #:with c::contract-infix-op+form+tail #'(:: . stxes)
             #:attr parsed #'c.parsed
             #:attr tail #'c.tail))


  (define-splicing-syntax-class :inline-contract
    #:datum-literals (op)
    #:literals (::)
    (pattern (~seq (op ::) ctc ...)
             #:with c::contract #'(group ctc ...)
             #:attr parsed #'c.parsed))

  (define-syntax-class :contract-form
    (pattern (predicate static-infos)))

  (define (contract-form predicate static-infos)
    #`(#,predicate #,static-infos))
  
  (define (identifier-contract name predicate-stx static-infos)
    (define packed #`(#,predicate-stx #,static-infos))
    (contract-prefix-operator
     name
     '((default . stronger))
     'macro
     (lambda (stx)
       (values packed (syntax-parse stx
                        [(_ . tail) #'tail]
                        [_ 'does-not-happen]))))))

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
        #:with c-parsed::contract-form #'t.parsed
        (values
         (wrap-static-info*
          #`(let ([val #,form])
              (if (c-parsed.predicate val)
                  val
                  (raise-contract-failure val 't.parsed)))
          #'c-parsed.static-infos)
         #'t.tail)]))
   ;; binding
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::contract-seq)
        #:with c-parsed::contract-form #'t.parsed
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
             #'(c-parsed.predicate
                c-parsed.static-infos
                left.arg-id))]
           [else (binding-form
                  #'left.arg-id
                  #'check-predicate-matcher
                  #'bind-nothing-new
                  #'(c-parsed.predicate
                     left.matcher-id
                     left.binder-id
                     left.data))])
         #'t.tail)]))))

(define-syntax is_a
  (expression-infix-operator
   #'::
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::contract-seq)
        #:with c-parsed::contract-form #'t.parsed
        (values
         #`(c-parsed.predicate #,form)
         #'t.tail)]))
   'none))

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

(define-syntax Integer (identifier-contract #'Integer #'exact-integer? #'()))
(define-syntax Number (identifier-contract #'Number #'number? #'()))
(define-syntax String (identifier-contract #'String #'string? #'()))

(define-syntax (define-contract-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-contract-space #'id)
         rhs)]))

(define (raise-contract-failure val ctc)
  (error '::
         (string-append "value does not match contract\n"
                        "  argument: ~v\n"
                        "  contract: ~s")
         val
         ctc))
