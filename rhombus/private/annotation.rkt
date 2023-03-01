#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     shrubbery/print
                     enforest
                     enforest/operator
                     enforest/syntax-local
                     enforest/property
                     enforest/proc-name
                     enforest/name-parse
                     enforest/operator
                     "srcloc.rkt"
                     "pack.rkt"
                     "introducer.rkt"
                     "annotation-string.rkt"
                     "realm.rkt"
                     "keyword-sort.rkt"
                     (for-syntax racket/base))
         "provide.rkt"
         "enforest.rkt"
         "annotation-operator.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "static-info.rkt"
         "parse.rkt"
         "realm.rkt"
         "parens.rkt")

(provide is_a
         (for-spaces (#f
                      rhombus/bind)
                     ::
                     :~)
         (for-space rhombus/annot

                    Boolean
                    PosInt
                    NegInt
                    NonnegInt
                    Integral
                    Rational
                    Exact
                    Inexact
                    Flonum
                    Byte
                    Number
                    Void
                    False

                    matching
                    #%parens
                    #%literal)
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Any
                     Real
                     Int))

(module+ for-class
  (begin-for-syntax
    (provide (property-out annotation-prefix-operator)
             (property-out annotation-infix-operator)

             identifier-annotation
             
             in-annotation-space
             annot-quote

             check-annotation-result

             :annotation
             :annotation-form
             :inline-annotation
             :unparsed-inline-annotation
             :annotation-infix-op+form+tail

             annotation-form

             parse-annotation-of))
  
  (provide define-annotation-syntax
           define-annotation-constructor

           raise-annotation-failure))

(begin-for-syntax
  ;; see also "annotation-property.rkt"

  (property annotation (predicate-stx static-infos))

  (define in-annotation-space (make-interned-syntax-introducer/add 'rhombus/annot))

  (define-syntax (annot-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/annot) #'id))]))

  (define (raise-not-a-annotation id)
    (raise-syntax-error #f
                        "not bound as an annotation"
                        id))

  (define (check-annotation-result stx proc)
    (unless (and (syntax? stx)
                 (let ([l (syntax->list stx)])
                   (and l
                        (= (length l) 2))))
      (raise-result-error* (proc-name proc)
                           rhombus-realm
                           "Annotation_Syntax"
                           stx))
    stx)

  (define-rhombus-enforest
    #:enforest enforest-annotation
    #:syntax-class :annotation
    #:infix-more-syntax-class :annotation-infix-op+form+tail
    #:desc "annotation"
    #:operator-desc "annotation operator"
    #:in-space in-annotation-space
    #:prefix-operator-ref annotation-prefix-operator-ref
    #:infix-operator-ref annotation-infix-operator-ref
    #:check-result check-annotation-result
    #:make-identifier-form raise-not-a-annotation)

  (define-syntax-class :annotation-seq
    (pattern stxes
             #:with (~var c (:annotation-infix-op+form+tail #'::)) #'(group . stxes)
             #:attr parsed #'c.parsed
             #:attr tail #'c.tail))

  (define-splicing-syntax-class :inline-annotation
    (pattern (~seq op::name ctc ...)
             #:do [(define check? (free-identifier=? (in-binding-space #'op.name) (bind-quote ::)))]
             #:when (or check?
                        (free-identifier=? (in-binding-space #'op.name) (bind-quote :~)))
             #:with c::annotation #'(group ctc ...)
             #:with c-parsed::annotation-form #'c.parsed
             #:attr predicate (if check? #'c-parsed.predicate #'#f)
             #:attr annotation-str (datum->syntax #f (shrubbery-syntax->string #'(ctc ...)))
             #:attr static-infos #'c-parsed.static-infos))

  (define-splicing-syntax-class :unparsed-inline-annotation
    #:attributes (seq)
    (pattern (~seq o::name ctc ...)
             #:when (or (free-identifier=? (in-binding-space #'o.name) (bind-quote ::))
                        (free-identifier=? (in-binding-space #'o.name) (bind-quote :~)))
             #:attr seq #'(o ctc ...)))

  (define-syntax-class :annotation-form
    (pattern (predicate static-infos)))

  (define (annotation-form predicate static-infos)
    #`(#,predicate #,static-infos))
  
  (define (identifier-annotation predicate-stx static-infos)
    (define packed #`(#,predicate-stx #,static-infos))
    (annotation-prefix-operator
     (quote-syntax ignored)
     '((default . stronger))
     'macro
     (lambda (stx)
       (values packed (syntax-parse stx
                        [(_ . tail) #'tail]
                        [_ 'does-not-happen])))))
  
  (define (parse-annotation-of stx predicate-stx static-infos
                               sub-n kws predicate-maker info-maker)
    (syntax-parse stx
      #:datum-literals (parens)
      [(form-id ((~and tag parens) g ...) . tail)
       (define unsorted-gs (syntax->list #'(g ...)))
       (unless (= (length unsorted-gs) sub-n)
         (raise-syntax-error #f
                             "wrong number of subannotations in parentheses"
                             #'(form-id (tag g ...))))
       (define gs (sort-with-respect-to-keywords kws unsorted-gs stx))
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
               #'tail)]))
     
  (define (annotation-constructor name predicate-stx static-infos
                                  sub-n kws predicate-maker info-maker
                                  parse-annotation-of)
    (values
     ;; root
     (annotation-prefix-operator
      name
      '((default . stronger))
      'macro
      (lambda (stx)
        (syntax-parse stx
          [(_ . tail)
           (values (annotation-form predicate-stx
                                    static-infos)
                   #'tail)])))
     ;; `of`:
     (annotation-prefix-operator
      name
      '((default . stronger))
      'macro
      (lambda (stx)
        (parse-annotation-of (replace-head-dotted-name stx)
                             predicate-stx static-infos
                             sub-n kws predicate-maker info-maker)))))

  (define (annotation-of-constructor name predicate-stx static-infos
                                     sub-n kws predicate-maker info-maker
                                     parse-annotation-of)
    (annotation-prefix-operator
      name
      '((default . stronger))
      'macro
      (lambda (stx)
        (syntax-parse stx
          #:datum-literals (op |.| parens of)
          [(form-id (op |.|) (~and of-id of) . tail)
           (parse-annotation-of #`(of-id . tail)
                                predicate-stx static-infos
                                sub-n kws predicate-maker info-maker)]
          [(form-id (op |.|) other:identifier . tail)
           (raise-syntax-error #f
                               "field not provided by annotation"
                               #'form-id
                               #'other)]
          [(_ . tail)
           ;; we don't get here when used specifically as `of`
           (values (annotation-form predicate-stx
                                    static-infos)
                   #'tail)])))))

(define-syntax (define-annotation-constructor stx)
  (syntax-parse stx
    [(_ (name of-name)
        binds
        predicate-stx static-infos
        sub-n kws predicate-maker info-maker
        (~optional (~seq #:parse-of parse-annotation-of-id)
                   #:defaults ([parse-annotation-of-id #'parse-annotation-of])))
     #:with annot-name (in-annotation-space #'name)
     #'(define-syntaxes (annot-name of-name)
         (let binds
             (annotation-constructor #'annot-<name predicate-stx static-infos
                                     sub-n 'kws predicate-maker info-maker
                                     parse-annotation-of-id)))]))

(define-for-syntax (make-annotation-apply-expression-operator name checked?)
  (expression-infix-operator
   name
   `((default . weaker))
   'macro
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
                      (raise-::-annotation-failure val '#,(shrubbery-syntax->string #'t))))
              form)
          #'c-parsed.static-infos)
         #'t.tail)]))
   'none))

(define-for-syntax (make-annotation-apply-binding-operator name checked?)
  (binding-infix-operator
   name
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op . t::annotation-seq)
        #:with c-parsed::annotation-form #'t.parsed
        #:with left::binding-form form
        (values
         (binding-form
          #'annotation-infoer
          #`(#,(shrubbery-syntax->string #'t)
             #,(and checked? #'c-parsed.predicate)
             c-parsed.static-infos
             left.infoer-id
             left.data))
         #'t.tail)]))
   'none))

(define-syntax ::
  (make-annotation-apply-expression-operator (expr-quote ::) #t))
(define-binding-syntax ::
  (make-annotation-apply-binding-operator (bind-quote ::) #t))

(define-syntax :~
  (make-annotation-apply-expression-operator (expr-quote :~) #f))
(define-binding-syntax :~
  (make-annotation-apply-binding-operator (bind-quote :~) #f))

(define-syntax is_a
  (expression-infix-operator
   (expr-quote is_a)
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
    [(_ static-infos (annotation-str predicate (static-info ...) left-infoer-id left-data))
     #:with left-impl::binding-impl #'(left-infoer-id (static-info ... . static-infos) left-data)
     #:with left::binding-info #'left-impl.info
     (if (syntax-e #'predicate)
         (binding-info (annotation-string-and (syntax-e #'left.annotation-str) (syntax-e #'annotation-str))
                       #'left.name-id
                       #'left.static-infos
                       #'left.bind-infos
                       #'check-predicate-matcher
                       #'commit-nothing-new
                       #'bind-nothing-new
                       #'(predicate left.matcher-id left.committer-id left.binder-id left.data))
         #'left)]))

(define-syntax (check-predicate-matcher stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-committer-id left-binder-id left-data) IF success fail)
     #'(IF (predicate arg-id)
           (left-matcher-id
            arg-id
            left-data
            IF
            success
            fail)
           fail)]))

(define-syntax (commit-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-committer-id left-binder-id left-data))
     #'(left-committer-id arg-id left-data)]))

(define-syntax (bind-nothing-new stx)
  (syntax-parse stx
    [(_ arg-id (predicate left-matcher-id left-committer-id left-binder-id left-data))
     #'(left-binder-id arg-id left-data)]))

(define-syntax (define-annotation-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-annotation-space #'id)
         rhs)]))

(define (exact-number? n) (and (number? n) (exact? n)))
(define (inexact-number? n) (and (number? n) (inexact? n)))
(define (exact-negative-integer? n) (and (integer? n) (exact? n) (negative? n)))

(define-annotation-syntax Any (identifier-annotation #'(lambda (x) #t) #'()))
(define-annotation-syntax Boolean (identifier-annotation #'boolean? #'()))
(define-annotation-syntax Int (identifier-annotation #'exact-integer? #'()))
(define-annotation-syntax PosInt (identifier-annotation #'exact-positive-integer? #'()))
(define-annotation-syntax NegInt (identifier-annotation #'exact-negative-integer? #'()))
(define-annotation-syntax NonnegInt (identifier-annotation #'exact-nonnegative-integer? #'()))
(define-annotation-syntax Flonum (identifier-annotation #'flonum? #'()))
(define-annotation-syntax Byte (identifier-annotation #'byte? #'()))
(define-annotation-syntax Number (identifier-annotation #'number? #'()))
(define-annotation-syntax Integral (identifier-annotation #'integer? #'()))
(define-annotation-syntax Rational (identifier-annotation #'rational? #'()))
(define-annotation-syntax Exact (identifier-annotation #'exact-number? #'()))
(define-annotation-syntax Inexact (identifier-annotation #'inexact-number? #'()))
(define-annotation-syntax Real (identifier-annotation #'real? #'()))
(define-annotation-syntax Void (identifier-annotation #'void? #'()))
(define-annotation-syntax False (identifier-annotation #'not #'()))

(define-name-root Any
  #:fields
  ([of Any.of]))

(define-name-root Int
  #:fields
  ([in Int.in]))

(define-name-root Real
  #:fields
  ([above Real.above]
   [at_least Real.at_least]
   [below Real.below]
   [at_most Real.at_most]
   [in Real.in]))

;; not exported, but referenced by `:annotation-seq` so that
;; annotation parsing terminates appropriately
(define-annotation-syntax ::
  (annotation-infix-operator
   (annot-quote ::)
   `((default . stronger))
   'macro
   (lambda (stx) (error "should not get here"))
   'none))

(define (raise-::-annotation-failure val ctc)
  (raise-annotation-failure ':: val ctc))

(define (raise-annotation-failure who val ctc)
  (raise
   (exn:fail:contract
    (error-message->adjusted-string
     who
     rhombus-realm
     (format
      (string-append "value does not satisfy annotation\n"
                     "  argument: ~v\n"
                     "  annotation: ~a")
      val
      (error-contract->adjusted-string
       ctc
       rhombus-realm))
     rhombus-realm)
    (current-continuation-marks))))

(define-annotation-syntax matching
  (annotation-prefix-operator
   (annot-quote matching)
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

(define-annotation-syntax #%parens
  (annotation-prefix-operator
   (annot-quote %parens)
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

(define-annotation-syntax #%literal
  (annotation-prefix-operator
   (annot-quote %literal)
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ . tail)
        (raise-syntax-error #f
                            "literal not allowed as an annotation"
                            #'tail)]))))

(define-for-syntax (make-unary-real-annotation id comp-stx)
  (annotation-prefix-operator
   id
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (_::parens n-g) . tail)
        (values #`((let ([n (rhombus-expression n-g)])
                     (lambda (v)
                       (and (real? v)
                            (#,comp-stx v n))))
                   ())
                #'tail)]))))

(define-annotation-syntax Real.above (make-unary-real-annotation (annot-quote Real.above) #'>))
(define-annotation-syntax Real.at_least (make-unary-real-annotation (annot-quote Real.above) #'>=))
(define-annotation-syntax Real.below (make-unary-real-annotation (annot-quote Real.below) #'<))
(define-annotation-syntax Real.at_most (make-unary-real-annotation (annot-quote Real.at_most) #'<=))

(begin-for-syntax
  (define-syntax-class :incl-group
    #:attributes (g comp)
    #:datum-literals (group)
    (pattern (group t ... #:exclusive)
             #:attr comp #'<
             #:attr g #'(group t ...))
    (pattern (group t ... #:inclusive)
             #:attr comp #'<=
             #:attr g #'(group t ...))
    (pattern g
             #:attr comp #'<=)))

(define-for-syntax (make-in-annotation name pred-stx)
  (annotation-prefix-operator
   name
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group)
       [(_ (_::parens lo::incl-group hi::incl-group)
           . tail)
        (values #`((let ([lo-v (rhombus-expression lo.g)]
                         [hi-v (rhombus-expression hi.g)])
                     (unless (#,pred-stx lo-v) (bad '#,name 'lower lo-v))
                     (unless (#,pred-stx hi-v) (bad '#,name 'upper hi-v))
                     (lambda (v)
                       (and (#,pred-stx v)
                            (lo.comp lo-v v)
                            (hi.comp v hi-v))))
                   ())
                #'tail)]))))

(define (bad who which v)
  (raise-argument-error* who rhombus-realm
                         (case who
                           [(Read.in) "Real"]
                           [else "Int"])
                         v))

(define-annotation-syntax Real.in
  (make-in-annotation
   (annot-quote Real.in)
   #'real?))

(define-annotation-syntax Int.in
  (make-in-annotation
   (annot-quote Int.in)
   #'exact-integer?))

(define-annotation-syntax Any.of
  (annotation-prefix-operator
   (annot-quote Any.of)
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       #:datum-literals (group)
       [(_ (_::parens g ...)
           . tail)
        (with-syntax ([(lit ...) (generate-temporaries #'(g ...))])
          (values #`((let ([lit (rhombus-expression g)]
                           ...)
                       (lambda (v)
                         (or (equal-always? lit v)
                             ...)))
                     ())
                  #'tail))]))))


