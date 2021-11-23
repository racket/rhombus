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
                     "tail.rkt"
                     "misuse.rkt"
                     "introducer.rkt")
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "static-info.rkt"
         "parse.rkt")

(provide ::
         -:
         is_a
         matching

         Any
         Boolean
         Integer
         Number
         String
         Symbol
         Keyword

         (for-space rhombus/annotation #%tuple))

(module+ for-class
  (begin-for-syntax
    (provide (property-out annotation-prefix-operator)
             (property-out annotation-infix-operator)

             identifier-annotation
             annotation-constructor
             
             in-annotation-space

             check-annotation-result

             :annotation
             :annotation-form
             :inline-annotation

             annotation-form))
  
  (provide define-annotation-syntax))

(begin-for-syntax
  (property annotation-prefix-operator prefix-operator
            #:property prop:procedure (make-raise-misuse "annotation"))
  (property annotation-infix-operator infix-operator
            #:property prop:procedure (make-raise-misuse "annotation"))

  (property annotation (predicate-stx static-infos))

  (define in-annotation-space (make-interned-syntax-introducer/add 'rhombus/annotation))

  (define (raise-not-a-annotation id)
    (raise-syntax-error #f
                        "not bound as a annotation"
                        id))

  (define (check-annotation-result stx proc)
    (unless (and (syntax? stx)
                 (let ([l (syntax->list stx)])
                   (and l
                        (= (length l) 2))))
      (raise-result-error (proc-name proc)
                          "annotation-syntax?"
                          stx))
    stx)

  (define-enforest
    #:enforest enforest-annotation
    #:syntax-class :annotation
    #:infix-more-syntax-class :annotation-infix-op+form+tail
    #:desc "annotation"
    #:operator-desc "annotation operator"
    #:in-space in-annotation-space
    #:name-path-op name-path-op
    #:prefix-operator-ref annotation-prefix-operator-ref
    #:infix-operator-ref annotation-infix-operator-ref
    #:check-result check-annotation-result
    #:make-identifier-form raise-not-a-annotation)

  (define-syntax-class :annotation-seq
    (pattern stxes
             #:with c::annotation-infix-op+form+tail #'(:: . stxes)
             #:attr parsed #'c.parsed
             #:attr tail #'c.tail))


  (define-splicing-syntax-class :inline-annotation
    #:datum-literals (op)
    #:literals (:: -:)
    (pattern (~seq (op ::) ctc ...)
             #:with c::annotation #'(group ctc ...)
             #:with c-parsed::annotation-form #'c.parsed
             #:attr predicate #'c-parsed.predicate
             #:attr static-infos #'c-parsed.static-infos)
    (pattern (~seq (op -:) ctc ...)
             #:with c::annotation #'(group ctc ...)
             #:with c-parsed::annotation-form #'c.parsed
             #:attr predicate #'#f
             #:attr static-infos #'c-parsed.static-infos))

  (define-syntax-class :annotation-form
    (pattern (predicate static-infos)))

  (define (annotation-form predicate static-infos)
    #`(#,predicate #,static-infos))
  
  (define (identifier-annotation name predicate-stx static-infos)
    (define packed #`(#,predicate-stx #,static-infos))
    (annotation-prefix-operator
     name
     '((default . stronger))
     'macro
     (lambda (stx)
       (values packed (syntax-parse stx
                        [(_ . tail) #'tail]
                        [_ 'does-not-happen])))))

  (define (annotation-constructor name predicate-stx static-infos
                                  sub-n predicate-maker info-maker)
    (annotation-prefix-operator
     name
     '((default . stronger))
     'macro
     (lambda (stx)
       (syntax-parse stx
         #:datum-literals (op |.| parens of)
         [(form-id (op |.|) of ((~and tag parens) g ...) . tail)
          (define gs (syntax->list #'(g ...)))
          (unless (= (length gs) sub-n)
            (raise-syntax-error #f
                                "wrong number of subannotations in parentheses"
                                #'form-id
                                #f
                                (list #'tag)))
          (define c-parseds (for/list ([g (in-list gs)])
                              (syntax-parse g
                                [c::annotation #'c.parsed])))
          (define c-predicates (for/list ([c-parsed (in-list c-parseds)])
                                 (syntax-parse c-parsed
                                   [c::annotation-form #'c.predicate])))
          (define c-static-infoss (for/list ([c-parsed (in-list c-parseds)])
                                    (syntax-parse c-parsed
                                      [c::annotation-form #'c.static-infos])))
          (values (annotation-form #`(lambda (v)
                                     (and (#,predicate-stx v)
                                          #,(predicate-maker #'v c-predicates)))
                                 #`(#,@(info-maker c-static-infoss)
                                    . #,static-infos))
                  #'tail)]
         [(_ . tail)
          (values (annotation-form predicate-stx
                                 static-infos)
                  #'tail)])))))

(define-for-syntax (make-annotation-apply-operator name checked?)
  (make-expression+binding-infix-operator
   name
   '((default . weaker))
   'macro
   'none
   ;; expression
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::annotation-seq)
        #:with c-parsed::annotation-form #'t.parsed
        (values
         (wrap-static-info*
          (if checked?
              #`(let ([val #,form])
                  (if (c-parsed.predicate val)
                      val
                      (raise-annotation-failure val 't.parsed)))
              form)
          #'c-parsed.static-infos)
         #'t.tail)]))
   ;; binding
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::annotation-seq)
        #:with c-parsed::annotation-form #'t.parsed
        #:with left::binding-form form
        (values
         (binding-form
          #'annotation-infoer
          #`(#,(and checked? #'c-parsed.predicate)
             c-parsed.static-infos
             left.infoer-id
             left.data))
         #'t.tail)]))))

(define-syntax ::
  (make-annotation-apply-operator #':: #t))

(define-syntax -:
  (make-annotation-apply-operator #'-: #f))

(define-syntax is_a
  (expression-infix-operator
   #'is_a
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::annotation-seq)
        #:with c-parsed::annotation-form #'t.parsed
        (values
         #`(c-parsed.predicate #,form)
         #'t.tail)]))
   'none))

(define-syntax (annotation-infoer stx)
  (syntax-parse stx
    [(_ static-infos (predicate (static-info ...) left-infoer-id left-data))
     #:with left-impl::binding-impl #'(left-infoer-id (static-info ... . static-infos) left-data)
     #:with left::binding-info #'left-impl.info
     (if (syntax-e #'predicate)
         (binding-info #'left.name-id
                       #'left.static-infos
                       #'left.bind-infos
                       #'check-predicate-matcher
                       #'bind-nothing-new
                       #'(predicate left.matcher-id left.binder-id left.data))
         #'left)]))

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

(define-syntax Any (identifier-annotation #'Any #'(lambda (x) #t) #'()))
(define-syntax Boolean (identifier-annotation #'Boolean #'boolean? #'()))
(define-syntax Integer (identifier-annotation #'Integer #'exact-integer? #'()))
(define-syntax Number (identifier-annotation #'Number #'number? #'()))
(define-syntax String (identifier-annotation #'String #'string? #'()))
(define-syntax Symbol (identifier-annotation #'Symbol #'symbol? #'()))
(define-syntax Keyword (identifier-annotation #'Keyword #'keyword? #'()))

(define-syntax (define-annotation-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-annotation-space #'id)
         rhs)]))

(define (raise-annotation-failure val ctc)
  (error '::
         (string-append "value does not match annotation\n"
                        "  argument: ~v\n"
                        "  annotation: ~s")
         val
         ctc))

(define-syntax matching
  (annotation-prefix-operator
   #'matching
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens)
       [(_ (parens arg::binding) . tail)
        #:with arg-parsed::binding-form #'arg.parsed
        #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
        #:with arg-info::binding-info #'arg-impl.info
        (values
         #`((lambda (arg-info.name-id)
              (arg-info.matcher-id arg-info.name-id
                                   arg-info.data
                                   if/blocked
                                   #t
                                   #f))
            arg-info.static-infos)
         #'tail)]))))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))

(define-annotation-syntax #%tuple
  (annotation-prefix-operator
   #'%tuple
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty annotation" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many annotations" #'head)]
            [else
             (syntax-parse (car args)
               [c::annotation (values #'c.parsed #'tail)])]))]))))
