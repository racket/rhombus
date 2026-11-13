#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "group.rkt"
                     "origin.rkt"
                     "split-at.rkt")
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
         "srcloc-error.rkt")

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
    #:attributes (bind-g rhs)
    #:datum-literals (group)
    (pattern (_::block (group bind ...+
                              (~and rhs (_::block . _))))
             #:with bind-g (regroup #'(bind ...))))

  ;; also checks consistent arity
  (define (extract-arity stx given-arity clauses b-parseds)
    (define (n->vals n)
      (string-append (number->string n)
                     (if (eqv? n 1)
                         " value"
                         " values")))
    (for/fold ([arity given-arity]
               #:result (or arity 1))
              ([clause (in-list clauses)]
               [b-parsed (in-list b-parseds)])
      (define current-arity (extract-binding-arity b-parsed))
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
              (define-values (clauses bs rhss else-expr)
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
                              (syntax->list #'(clause.bind-g ...))
                              (syntax->list #'(clause.rhs ...))
                              (and (attribute e)
                                   #'(rhombus-body-expression e.rhs)))])]
                  [else
                   (values '() '() '() #f)]))]
        #:with (b::binding ...) bs
        #:do [(define in-expr (regroup #'(in ...)))
              (define b-parseds (syntax->list #'(b.parsed ...)))
              (define arity (extract-arity stx given-arity clauses b-parseds))]
        (values
         (relocate+reraw
          (respan stx)
          (bind-target-expr
           arity in-expr
           (lambda (val-ids statinfoss)
             (define (do-handle-normal-match b-parseds rhss)
               (handle-normal-match val-ids statinfoss arity
                                    b-parseds rhss
                                    (or else-expr
                                        #`(match-fallthrough 'form-id '#,(syntax-srcloc (respan stx))))))
             (define (do-handle-literal-case-dispatch)
               (handle-literal-case-dispatch val-ids
                                             b-parseds rhss
                                             ;; `fallback-k` is called with the remaining non-literal patterns
                                             do-handle-normal-match))
             (define (do-handle-syntax-parse-dispatch)
               (handle-syntax-parse-dispatch val-ids (syntax-e #'form-id)
                                             b-parseds rhss
                                             (lambda ()
                                               #`(not-syntax 'form-id '#,(syntax-srcloc (respan stx))))
                                             do-handle-literal-case-dispatch))
             (if (eqv? arity 1)
                 (if (not else-expr)
                     (do-handle-syntax-parse-dispatch)
                     (do-handle-literal-case-dispatch))
                 (do-handle-normal-match b-parseds rhss)))))
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

(define-for-syntax (handle-normal-match val-ids statinfoss arity
                                        b-parseds rhss
                                        else-expr)
  (for/foldr ([next else-expr])
             ([b-parsed (in-list b-parseds)]
              [rhs (in-list rhss)])
    (syntax-parse b-parsed
      [b-parsed::binding-values-form
       #:with (val-id ...) val-ids
       #:with (statinfos ...) statinfoss
       #:with (~var b-impl (:binding-impl arity)) #'(b-parsed.infoer-id (statinfos ...) b-parsed.data)
       #:with b-info::binding-info #'b-impl.info
       (disallow-binding-as-namespace-extension #'b-info)
       (transfer-origin
        #'b-parsed
        ;; use `((lambda ....) ....)` to keep textual order
        #`((lambda (try-next)
             (b-info.oncer-id b-info.data)
             (b-info.matcher-id (val-id ...)
                                b-info.data
                                if/blocked
                                (begin
                                  (b-info.committer-id (val-id ...) b-info.evidence-ids b-info.data)
                                  (b-info.binder-id (val-id ...) b-info.evidence-ids b-info.data)
                                  (define-static-info-syntax/maybe b-info.bind-id b-info.bind-static-info ...)
                                  ...
                                  (rhombus-body-expression #,rhs))
                                (try-next)))
           (lambda () #,next)))])))

(define-for-syntax (handle-literal-case-dispatch val-ids
                                                 b-parseds rhss
                                                 fallback-k)
  (define (literal-binding? parsed)
    (syntax-parse parsed
      [b::binding-form
       (free-identifier=? #'b.infoer-id #'literal-infoer)]))
  ;; index at which the initial literal segment ends, if any
  (define maybe-idx
    (for/fold ([maybe-idx #f])
              ([parsed (in-list b-parseds)]
               [idx (in-naturals 1)])
      (unless (eqv? (extract-binding-arity parsed) 1)
        (error "handle-literal-case-dispatch: must only apply to single-value pattern"))
      #:break (not (literal-binding? parsed))
      idx))
  (cond
    [maybe-idx
     (define-values (lit-parseds rst-parseds) (split-at b-parseds maybe-idx))
     (unless (or (null? rst-parseds)
                 (not (literal-binding? (car rst-parseds))))
       (error "handle-literal-case-dispatch: literal bindings left over"))
     (define-values (lit-rhss rst-rhss) (split-at rhss maybe-idx))
     #`(case/equal-always #,(car val-ids)
         #,@(for/list ([parsed (in-list lit-parseds)]
                       [rhs (in-list lit-rhss)])
              (syntax-parse parsed
                [b::binding-form
                 #:with ([datum _] ...) #'b.data
                 #`[(datum ...) (rhombus-body-expression #,rhs)]]))
         [else #,(fallback-k rst-parseds rst-rhss)])]
    [else
     (fallback-k b-parseds rhss)]))

(define (match-fallthrough who loc)
  (raise-srcloc-error who "no matching case" loc))

(define (not-syntax who loc)
  (raise-srcloc-error who "expected a syntax object" loc))

(define-for-syntax (parse-matches form tail mode)
  (syntax-parse tail
    [(op . tail)
     #:with (~var t (:infix-op+binding+tail #'matches)) (regroup #`tail)
     (values
      (syntax-parse #'t.parsed
        [b::binding-form
         #:with b-impl::binding-impl #'(b.infoer-id () b.data)
         #:with b-info::binding-info #'b-impl.info
         (disallow-binding-as-namespace-extension #'b-impl.info)
         (transfer-origin
          #'t.parsed
          #`(let ([val-in (let ([b-info.name-id #,(discard-static-infos form)])
                            b-info.name-id)])
              (b-info.oncer-id b-info.data)
              (b-info.matcher-id val-in
                                 b-info.data
                                 if/blocked
                                 #,(eq? mode 'normal)
                                 #,(not (eq? mode 'normal)))))])
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
