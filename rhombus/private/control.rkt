#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt"
                     (submod "entry-point-adjustment.rkt" for-struct))
         syntax/parse/pre
         "provide.rkt"
         "name-root.rkt"
         "parens.rkt"
         "expression.rkt"
         "binding.rkt"
         "entry-point.rkt"
         "parse.rkt"
         "realm.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class)
         (submod "function-parse.rkt" for-build)
         (submod "equal.rkt" for-parse)
         "if-blocked.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Continuation)
         try
         throw
         (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-error error])))

(define-name-root Continuation
  #:fields
  (Marks
   PromptTag
   current_marks
   capture
   prompt
   escape
   default_prompt_tag
   make_prompt_tag
   call_in
   with_mark
   call_with_immediate_mark))

(define-annotation-syntax Continuation (identifier-annotation #'continuation? #'()))

(define-annotation-syntax PromptTag (identifier-annotation #'continuation-prompt-tag? #'()))

(define-annotation-syntax Marks (identifier-annotation #'continuation-mark-set? #'()))

(define/arity (current_marks)
  (current-continuation-marks))

(define/arity #:name error rhombus-error
  (case-lambda
    [(msg) (do-error who #f msg)]
    [(who-in msg) (do-error who who-in msg)]))

(define (do-error e-who who msg)
  (define who-sym
    (cond
      [(not who) #f]
      [(symbol? who) who]
      [(string? who) (string->symbol who)]
      [(identifier? who) (syntax-e who)]
      [(and (syntax? who)
            (syntax-parse who
              #:datum-literals (op)
              [(op id) (syntax-e #'id)]
              [_ #f]))]
      [else (raise-argument-error* e-who
                                   rhombus-realm
                                   "maybe(ReadableString || Symbol || Identifier || Operator)"
                                   who)]))
  (unless (string? msg)
    (raise-argument-error* e-who rhombus-realm "ReadableString" msg))
  (define adj (current-error-message-adjuster))
  (define-values (adj-who who-realm)
    (if who-sym
        ((or (adj 'name) values) who-sym rhombus-realm)
        (values #f rhombus-realm)))
  (define-values (err-who error-who-realm adj-msg msg-realm)
    ((or (adj 'message) values) adj-who who-realm msg rhombus-realm))
  (if err-who
      (error err-who "~a" adj-msg)
      (error adj-msg)))

(define-syntax try
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block g ...+))
        (define gs (syntax->list #'(g ...)))
        (define-values (rev-gs state)
          (let loop ([gs gs] [rev-gs null] [state #hasheq()])
            (cond
              [(null? gs) (values rev-gs state)]
              [else
               (syntax-parse (car gs)
                 #:datum-literals (group)
                 [(group #:initially . _)
                  #:when (hash-ref state 'initially #f)
                  (raise-syntax-error #f "duplicate `~initially` clause" stx (car gs))]
                 [(group #:initially . _)
                  #:when (or (pair? rev-gs) (positive? (hash-count state)))
                  (raise-syntax-error #f "`~initially` clause must appear at the start of the body" stx (car gs))]
                 [(group #:initially (tag::block body ...+))
                  (loop (cdr gs) rev-gs (hash-set state 'initially #'(rhombus-body-at tag body ...)))]
                 [(group #:initially term ...+)
                  (loop (cdr gs) rev-gs (hash-set state 'initially #'(rhombus-expression (group term ...))))]
                 [(group (~and kw #:initially) . _)
                  (raise-syntax-error #f "expected block or expression after `~initially`" stx #'kw)]
                 [(group #:finally . _)
                  #:when (hash-ref state 'finally #f)
                  (raise-syntax-error #f "duplicate `~finally` clause" stx (car gs))]
                 [(group #:finally (tag::block body ...+))
                  (loop (cdr gs) rev-gs (hash-set state 'finally #'(rhombus-body-at tag body ...)))]
                 [(group #:finally term ...+)
                  (loop (cdr gs) rev-gs (hash-set state 'finally #'(rhombus-expression (group term ...))))]
                 [(group (~and kw #:finally) . _)
                  (raise-syntax-error #f "expected block or expression after `~finally`" stx #'kw)]
                 [(group #:catch . _)
                  #:when (hash-ref state 'handler #f)
                  (raise-syntax-error #f "duplicate `~catch` clause" stx (car gs))]
                 [(group #:catch . _)
                  #:when (hash-ref state 'finally #f)
                  (raise-syntax-error #f "`~catch` not allowed after `~finally`" stx (car gs))]
                 [(group #:catch (_::alts b ...))
                  (define handler
                    (let catch-loop ([bs (syntax->list #'(b ...))])
                      (cond
                        [(null? bs) #'raise]
                        [else
                         (syntax-parse (car bs)
                           [(_ (_ bind ...+ (tag::block body ...+)))
                            (binding-to-function #`(#,group-tag bind ...)
                                                 #'(rhombus-body-at tag body ...)
                                                 (lambda ()
                                                   (catch-loop (cdr bs))))]
                           [_ (raise-syntax-error #f
                                                  "expected a binding and non-empty block for `~catch` alternative"
                                                  stx
                                                  (car bs))])])))
                  (loop (cdr gs) rev-gs (hash-set state 'handler handler))]
                 [(group #:catch bind ...+ (tag::block body ...+))
                  (loop (cdr gs) rev-gs (hash-set state 'handler (binding-to-function #`(#,group-tag bind ...)
                                                                                      #'(rhombus-body-at tag body ...)
                                                                                      (lambda () #'raise))))]
                 [(group (~and kw #:catch) . _)
                  (raise-syntax-error #f "expected alternatives or a binding and non-empty block after `~catch`" stx #'kw)]
                 [_
                  (when (or (hash-ref state 'handler #f)
                            (hash-ref state 'finally #f))
                    (raise-syntax-error #f "expression or definition not allowed after `~catch` or `~finally`"
                                        stx
                                        (car gs)))
                  (loop (cdr gs) (cons (car gs) rev-gs) state)])])))
        (let* ([body #`(rhombus-body-at tag #,@(reverse rev-gs))]
               [body (let ([handler (hash-ref state 'handler #f)])
                       (if handler
                           #`(with-handlers ([always-true #,handler])
                               #,body)
                           body))]
               [body (let ([initially (hash-ref state 'initially #f)]
                           [finally (hash-ref state 'finally #f)])
                       (if (or finally initially)
                           #`(dynamic-wind
                                     #,(if initially #`(lambda () #,initially) #'void)
                               (lambda () #,body)
                               #,(if finally #`(lambda () #,finally) #'void))
                           body))])
          (values body #'()))]))))

(define-syntax throw
  (expression-prefix-operator
   (expr-quote throw)
   '((default . weaker))
   'automatic
   (lambda (form1 op-stx)
     #`(raise #,form1))))

(define-syntax capture
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier (tag::block g ...))
        (values #'(call-with-composable-continuation
                   (lambda (id)
                     (rhombus-body-at tag g ...)))
                #'())]
       [(_ tag-expr ...+ id:identifier (tag::block g ...))
        (values #`(call-with-composable-continuation
                   (lambda (id)
                     (rhombus-body-at tag g ...))
                   (rhombus-expression (#,group-tag tag-expr ...)))
                #'())]))))

(define-syntax prompt
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ tag-expr ... (tag::block g ...+))
        (define-values (rev-gs state)
          (let loop ([gs (syntax->list #'(g ...))] [rev-gs null] [state #hasheq()])
            (cond
              [(null? gs) (values rev-gs state)]
              [else
               (syntax-parse (car gs)
                 #:datum-literals (group)
                 [(group #:catch . _)
                  #:when (hash-ref state 'handler #f)
                  (raise-syntax-error #f "duplicate `~catch` clause" stx (car gs))]
                 [(group #:catch (_::alts b ...))
                  (define handler
                    (let catch-loop ([bs (syntax->list #'(b ...))]
                                     [rev-argss null]
                                     [rev-arg-parsedss null]
                                     [rev-rhss null])
                      (cond
                        [(null? bs)
                         (define argss (reverse rev-argss))
                         (define arg-parsedss (reverse rev-arg-parsedss))
                         (define rhss (reverse rev-rhss))
                         (define falses (map (lambda (f) #f) argss))
                         (define falses-stx (datum->syntax #f (map (lambda (f) #'#f) argss)))
                         (define-values (proc arity)
                           (build-case-function no-adjustments
                                                #'prompt_handler
                                                #f #f
                                                (datum->syntax #f
                                                               (for/list ([args (in-list argss)])
                                                                 (for/list ([arg (in-list (syntax->list args))])
                                                                   #'#f)))
                                                (datum->syntax #f argss) (datum->syntax #f arg-parsedss)
                                                falses-stx falses-stx
                                                falses-stx falses-stx
                                                falses falses
                                                (datum->syntax #f rhss)
                                                (car gs)))
                         proc]
                        [else
                         (syntax-parse (car bs)
                           [(_ (_ (parens-tag::parens arg::binding ...) (~and rhs (_::block body ...+))))
                            (catch-loop (cdr bs)
                                        (cons #'(arg ...) rev-argss)
                                        (cons #'(arg.parsed ...) rev-arg-parsedss)
                                        (cons #'rhs rev-rhss))]
                           [(_ (_ bind ...+ (~and rhs (_::block body ...+))))
                            #:with arg::binding #`(#,group-tag bind ...)
                            (catch-loop (cdr bs)
                                        (cons #'(arg) rev-argss)
                                        (cons #'(arg.parsed) rev-arg-parsedss)
                                        (cons #'rhs rev-rhss))]
                           [_ (raise-syntax-error #f
                                                  "expected a binding and non-empty block for `~catch` alternative"
                                                  stx
                                                  (car bs))])])))
                  (loop (cdr gs) rev-gs (hash-set state 'handler handler))]
                 [(group #:catch bind ...+ (~and rhs (tag::block body ...+)))
                  (define handler
                    (syntax-parse #`(#,group-tag bind ...)
                      [(parens-tag::parens arg::binding ...)
                       (define falses (datum->syntax #f (map (lambda (a) #'#f) (syntax->list #'(arg ...)))))
                       (define-values (proc arity)
                         (build-function no-adjustments
                                         #'prompt_handler
                                         falses #'(arg ...) #'(arg.parsed ...) falses
                                         #'#f #'#f
                                         #'#f #'#f
                                         #f #f
                                         #'rhs
                                         (car gs)))
                       proc]
                      [arg::binding
                       (define-values (proc arity)
                         (build-function no-adjustments
                                         #'prompt_handler
                                         #'(#f) #'(arg) #'(arg.parsed) #'(#f)
                                         #'#f #'#f
                                         #'#f #'#f
                                         #f #f
                                         #'rhs
                                         (car gs)))
                       proc]))
                  (loop (cdr gs) rev-gs (hash-set state 'handler handler))]
                 [(group #:catch (tag::block entry))
                  #:with (~var e (:entry-point no-adjustments)) #'entry
                  (loop (cdr gs) rev-gs (hash-set state 'handler #'e.parsed))]
                 [(group #:catch . _)
                  (raise-syntax-error #f "expected a non-empty block after `~catch`" stx #'kw)]
                 [_
                  (when (hash-ref state 'handler #f)
                    (raise-syntax-error #f "expression or definition not allowed after `~catch`"
                                        stx
                                        (car gs)))
                  (loop (cdr gs) (cons (car gs) rev-gs) state)])])))
        (values #`(call-with-continuation-prompt
                   (lambda () (rhombus-body-at tag #,@(reverse rev-gs)))
                   #,(if (null? (syntax-e #'(tag-expr ...)))
                         #'(default-continuation-prompt-tag)
                         #'(rhombus-expression (group tag-expr ...)))
                   #,@(let ([handler (hash-ref state 'handler #f)])
                        (if handler
                            (list handler)
                            null)))
                #'())]))))

(define/arity (escape #:tag [prompt-tag (default-continuation-prompt-tag)] . vals)
  (apply abort-current-continuation prompt-tag vals))

(define default_prompt_tag (default-continuation-prompt-tag))

(define/arity (make_prompt_tag [name-in #f])
  (define name
    (cond
      [(not name-in) #f]
      [(symbol? name-in) name-in]
      [(string? name-in) (string->symbol name-in)]
      [else (raise-argument-error* who
                                   rhombus-realm
                                   "maybe(ReadableString || Symbol)"
                                   name-in)]))
  (if name
      (make-continuation-prompt-tag name)
      (make-continuation-prompt-tag)))

(define/arity (call_in k proc)
  (unless (continuation? k)
    (raise-argument-error* who
                           rhombus-realm
                           "Continuation"
                           k))
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 0))
    (raise-argument-error* who
                           rhombus-realm
                           "Function.of_arity(0)"
                           proc))
  (call-in-continuation k proc))

(define-for-syntax (binding-to-function binds body get-fail)
  (syntax-parse binds
    [b::binding
     #:with arg-parsed::binding-form #'b.parsed
     #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
     #:with arg::binding-info #'arg-impl.info
     #`(lambda (arg-id)
         (arg.matcher-id arg-id arg.data
                         if/blocked
                         (let ()
                           (arg.committer-id arg-id arg.data)
                           (arg.binder-id arg-id arg.data)
                           #,body)
                         (#,(get-fail) arg-id)))]))

(define (always-true x) #t)

(define-syntax with_mark
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ ... a::equal _ ... b::equal . _)
        (raise-too-many-equals stx #'a #'b)]
       [(_ lhs ... _::equal rhs ... (tag::block g ...))
        (values #'(with-continuation-mark
                    (rhombus-expression (group lhs ...))
                    (rhombus-expression (group rhs ...))
                    (rhombus-body-at tag g ...))
                #'())]))))

(define/arity (call_with_immediate_mark key proc #:default [default #f])
  (call-with-immediate-continuation-mark key proc default))
