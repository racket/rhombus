#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest
                     enforest/operator
                     enforest/syntax-local
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
         "static-info.rkt"
         "bind-input-key.rkt"
         "parse.rkt")

(provide ::
         is_a
         matching

         Integer
         Number
         String

         (for-space rhombus/contract #%tuple))

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
         (binding-form
          #'left.arg-id
          #'c-parsed.static-infos
          (extend-bind-input #'c-parsed.static-infos (syntax->list #'left.bind-ids))
          #'check-predicate-matcher
          #'bind-nothing-new
          #'(c-parsed.predicate
             left.matcher-id
             left.binder-id
             left.data))
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

(define-syntax matching
  (contract-prefix-operator
   #'matching
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens)
       [(_ (parens arg::binding) . tail)
        #:with arg-parsed::binding-form #'arg.parsed
        (values
         #`((lambda (arg-parsed.arg-id)
              (arg-parsed.matcher-id arg-parsed.arg-id
                                     arg-parsed.data
                                     if/blocked
                                     #t
                                     #f))
            arg-parsed.static-infos)
         #'tail)]))))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))

(define-contract-syntax #%tuple
  (contract-prefix-operator
   #'%tuple
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty contract" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many contracts" #'head)]
            [else
             (syntax-parse (car args)
               [c::contract (values #'c.parsed #'tail)])]))]))))
