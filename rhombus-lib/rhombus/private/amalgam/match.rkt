#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         (only-in racket/case
                  case/equal-always)
         "expression.rkt"
         "repetition.rkt"
         "binding.rkt"
         "parse.rkt"
         "else-clause.rkt"
         "parens.rkt"
         (submod "quasiquote.rkt" for-match)
         (only-in "literal.rkt" literal-infoer)
         "if-blocked.rkt"
         "static-info.rkt"
         "order.rkt"
         "order-primitive.rkt"
         "provide.rkt"
         "srcloc-error.rkt"
         (submod "values.rkt" for-parse))

(provide match
         (for-spaces (#f
                      rhombus/repet)
                     matches))

(begin-for-syntax
  (define-syntax-class :arity-decl
    #:description "arity declaration"
    #:opaque
    #:attributes (num)
    #:datum-literals (group)
    (pattern (group #:arity (_::block (group num:exact-nonnegative-integer))))
    (pattern (group #:arity num:exact-nonnegative-integer)))

  (define-syntax-class :pattern-clause
    #:description "pattern clause"
    #:opaque
    #:attributes ([bind-g 1] rhs)
    #:datum-literals (group)
    (pattern (_::block (group (~optional _::values-id-bind) (_::parens bind-g ...)
                              (~and rhs (_::block . _)))))
    (pattern (_::block (group bind ...+
                              (~and rhs (_::block . _))))
             #:with (bind-g ...) #`((#,group-tag bind ...))))

  ;; also checks consistent arity
  (define (extract-arity stx given-arity clauses bss)
    (define (n->vals n)
      (string-append (number->string n)
                     (if (eqv? n 1)
                         " value"
                         " values")))
    (for/fold ([arity given-arity]
               #:result (or arity 1))
              ([clause (in-list clauses)]
               [bs (in-list bss)])
      (define current-arity (length bs))
      (unless (or (not arity)
                  (eqv? current-arity arity))
        (raise-syntax-error
         #f
         (if given-arity
             (string-append "current clause is inconsistent with declared arity;"
                            "\n arity is declared to be " (number->string given-arity)
                            ", but current clause matches " (n->vals current-arity))
             (string-append "all clauses must match the same number of values;"
                            "\n previous clauses match " (n->vals arity)
                            ", but current clause matches " (n->vals current-arity)))
         stx
         clause))
      current-arity)))

(define-syntax match
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(~or* (form-id in ...+
                       (~and (_::block . _) arity-b)
                       (~and (_::alts . _) clauses-a))
              (form-id in ...+
                       (~and (_::alts . _) clauses-a))
              (form-id in ...+
                       (_::block))
              (form-id in ...+
                       (~and (_::block . _) arity-b)))
        #:do [(define given-arity
                (and (attribute arity-b)
                     (syntax-parse #'arity-b
                       #:context stx
                       [(_ arity-g::arity-decl) (syntax-e #'arity-g.num)])))
              (define-values (clauses bss rhss else-expr)
                (cond
                  [(attribute clauses-a)
                   (syntax-parse #'clauses-a
                     #:context stx
                     [(~or* (_ clause::pattern-clause
                               ...
                               e::else-clause)
                            (_ clause::pattern-clause
                               ...))
                      (values (syntax->list #'(clause ...))
                              (map syntax->list (syntax->list #'((clause.bind-g ...) ...)))
                              (syntax->list #'(clause.rhs ...))
                              (and (attribute e)
                                   #'(rhombus-body-expression e.rhs)))])]
                  [else
                   (values '() '() '() #f)]))
              (define arity (extract-arity stx given-arity clauses bss))]
        #:with ((b::binding ...) ...) bss
        (define in-expr #`(#,group-tag in ...))
        (define b-parsedss (map syntax->list (syntax->list #'((b.parsed ...) ...))))
        (values
         (relocate+reraw
          (respan stx)
          (bind-target-expr
           arity in-expr
           (lambda (val-ids statinfoss)
             (define (do-handle-normal-match b-parsedss rhss)
               (handle-normal-match val-ids statinfoss
                                    b-parsedss rhss
                                    (or else-expr
                                        #`(match-fallthrough 'form-id '#,(syntax-srcloc (respan stx))))))
             (define (do-handle-literal-case-dispatch)
               (handle-literal-case-dispatch val-ids b-parsedss rhss
                                             ;; `fallback-k` is called with the remaining non-literal patterns
                                             do-handle-normal-match))
             (define (do-handle-syntax-parse-dispatch)
               (handle-syntax-parse-dispatch #'form-id
                                             val-ids b-parsedss rhss
                                             (lambda ()
                                               #`(not-syntax 'form-id '#,(syntax-srcloc (respan stx))))
                                             do-handle-literal-case-dispatch))
             (if (eqv? arity 1)
                 (if (not else-expr)
                     (do-handle-syntax-parse-dispatch)
                     (do-handle-literal-case-dispatch))
                 (do-handle-normal-match b-parsedss rhss)))))
         #'())]))))

(define-for-syntax (bind-target-expr arity in-expr
                                     build-body-k)
  (define val-ids (generate-temporaries
                   (for/list ([_ (in-range arity)])
                     'val)))
  (define-values (parsed-expr statinfoss)
    (syntax-parse in-expr
      [e::expression
       (define parsed-expr (rhombus-local-expand #'e.parsed))
       (values (discard-static-infos parsed-expr)
               (normalize-static-infos/values
                arity
                (extract-static-infos parsed-expr)))]))
  #`(let-values ([(#,@val-ids) #,parsed-expr])
      #,(build-body-k val-ids statinfoss)))

(define-for-syntax (handle-normal-match val-ids statinfoss
                                        b-parsedss rhss
                                        else-expr)
  (for/foldr ([next else-expr])
             ([b-parseds (in-list b-parsedss)]
              [rhs (in-list rhss)])
    (syntax-parse b-parseds
      [(b-parsed::binding-form ...)
       #:with (val-id ...) val-ids
       #:with (statinfos ...) statinfoss
       #:with (b-impl::binding-impl ...) #'((b-parsed.infoer-id statinfos b-parsed.data) ...)
       #:with (b-info::binding-info ...) #'(b-impl.info ...)
       (for ([b-info (in-list (syntax->list #'(b-impl.info ...)))])
         (disallow-binding-as-namespace-extension b-info))
       (define rhs-body
         #`(begin
             (b-info.committer-id val-id b-info.evidence-ids b-info.data)
             ...
             (b-info.binder-id val-id b-info.evidence-ids b-info.data)
             ...
             (define-static-info-syntax/maybe b-info.bind-id b-info.bind-static-info ...)
             ... ...
             (rhombus-body-expression #,rhs)))
       ;; use `((lambda ....) ....)` to keep textual order
       #`((lambda (try-next)
            (b-info.oncer-id b-info.data)
            ...
            #,(for/foldr ([success rhs-body])
                         ([b-info-matcher-id (in-list (syntax->list #'(b-info.matcher-id ...)))]
                          [val-id (in-list val-ids)]
                          [b-info-data (in-list (syntax->list #'(b-info.data ...)))])
                #`(#,b-info-matcher-id #,val-id
                                       #,b-info-data
                                       if/blocked
                                       #,success
                                       (try-next))))
          (lambda () #,next))])))

(define-for-syntax (handle-literal-case-dispatch val-ids
                                                 b-parsedss rhss
                                                 fallback-k)
  (define (split-at lst idx)
    (cond
      [(eqv? idx 0)
       (values '() lst)]
      [else
       (define-values (l-lst r-lst) (split-at (cdr lst) (sub1 idx)))
       (values (cons (car lst) l-lst) r-lst)]))
  (define (literal-binding? parsed)
    (syntax-parse parsed
      [b::binding-form
       (free-identifier=? #'b.infoer-id #'literal-infoer)]))
  ;; index at which the initial literal segment ends, if any
  (define maybe-idx
    (for/fold ([maybe-idx #f])
              ([parseds (in-list b-parsedss)]
               [idx (in-naturals 1)])
      (unless (null? (cdr parseds))
        (error "handle-literal-case-dispatch: must only apply to single-value pattern"))
      #:break (not (literal-binding? (car parseds)))
      idx))
  (cond
    [maybe-idx
     (define-values (lit-parsedss rst-parsedss) (split-at b-parsedss maybe-idx))
     (unless (or (null? rst-parsedss)
                 (not (literal-binding? (caar rst-parsedss))))
       (error "handle-literal-case-dispatch: literal bindings left over"))
     (define-values (lit-rhss rst-rhss) (split-at rhss maybe-idx))
     #`(case/equal-always #,(car val-ids)
         #,@(for/list ([parseds (in-list lit-parsedss)]
                       [rhs (in-list lit-rhss)])
              (syntax-parse (car parseds)
                [b::binding-form
                 #:with ([datum _] ...) #'b.data
                 #`[(datum ...) (rhombus-body-expression #,rhs)]]))
         [else #,(fallback-k rst-parsedss rst-rhss)])]
    [else
     (fallback-k b-parsedss rhss)]))

(define (match-fallthrough who loc)
  (raise-srcloc-error who "no matching case" loc))

(define (not-syntax who loc)
  (raise-srcloc-error who "expected a syntax object" loc))

(define-for-syntax (parse-matches form tail mode)
  (syntax-parse tail
    [(op . tail)
     #:with (~var t (:infix-op+binding+tail #'matches)) #`(#,group-tag . tail)
     (values
      (syntax-parse #'t.parsed
        [b::binding-form
         #:with b-impl::binding-impl #'(b.infoer-id () b.data)
         #:with b-info::binding-info #'b-impl.info
         (disallow-binding-as-namespace-extension #'b-impl.info)
         #`(let ([val-in (let ([b-info.name-id #,(discard-static-infos form)])
                           b-info.name-id)])
             (b-info.oncer-id b-info.data)
             (b-info.matcher-id val-in
                                b-info.data
                                if/blocked
                                #,(eq? mode 'normal)
                                #,(not (eq? mode 'normal))))])
      #'t.tail)]))

(define-syntax matches
  (expression-infix-operator
   (lambda () (order-quote equivalence))
   `()
   'macro
   (lambda (form tail [mode 'normal])
     (parse-matches form tail mode))
   'none))

(define-repetition-syntax matches
  (repetition-infix-operator
   (lambda () (order-quote equivalence))
   `()
   'macro
   (lambda (form tail [mode 'normal])
     (syntax-parse tail
       [(self . _)
        (syntax-parse form
          [rep::repetition-info
           (define-values (body new-tail) (parse-matches #'rep.body tail mode))
           (values
            (make-repetition-info #'self
                                  #'rep.for-clausess
                                  body
                                  #'()
                                  #'rep.used-depth)
            new-tail)])]))
   'none))

;; for precedence
(define-binding-syntax matches
  (binding-infix-operator
   #f
   `((default . stronger))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))
