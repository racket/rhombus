#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt"
                     "entry-point-adjustment.rkt")
         "provide.rkt"
         "name-root.rkt"
         "parens.rkt"
         "expression.rkt"
         "binding.rkt"
         "entry-point.rkt"
         "parse.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class)
         (submod "function-parse.rkt" for-build)
         (submod "equal.rkt" for-parse)
         "function-arity-key.rkt"
         "static-info.rkt"
         (submod "function.rkt" for-info)
         "if-blocked.rkt"
         "rhombus-primitive.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Continuation)
         try
         throw)

(define-name-root Continuation
  #:fields
  (Marks
   PromptTag
   [current_marks Continuation.Marks.current #:deprecate (#f rhombus/statinfo) "15-Apr-2025"]
   capture
   in
   prompt
   barrier
   [escape Continuation.escape]
   [default_prompt_tag Continuation.PromptTag.default #:deprecate (#f rhombus/statinfo) "15-Apr-2025"]
   [make_prompt_tag Continuation.PromptTag.make]
   [call_in call-in-continuation  #:deprecate (#f rhombus/statinfo) "15-Apr-2025"]
   with_mark
   [call_with_immediate_mark Continuation.call_with_immediate_mark]))

(define-name-root PromptTag
  #:fields
  ([default Continuation.PromptTag.default]
   [make Continuation.PromptTag.make]))

(define-name-root Marks
  #:fields
  ([current Continuation.Marks.current]))

(define-static-info-getter get-continuation-static-infos
  (#%function-arity -1)
  . #,(get-function-static-infos))

(void (set-primitive-contract! 'continuation? "Continuation"))
(define-annotation-syntax Continuation (identifier-annotation continuation? #,(get-continuation-static-infos)))

(void (set-primitive-contract! 'continuation-prompt-tag? "Continuation.PromptTag"))
(define-annotation-syntax PromptTag (identifier-annotation continuation-prompt-tag? ()))

(void (set-primitive-contract! 'continuation-mark-set? "Continuation.Marks"))
(define-annotation-syntax Marks (identifier-annotation continuation-mark-set? ()))

(define/arity (Continuation.Marks.current)
  (current-continuation-marks))

(define-for-syntax (parse-try stx now-stx)
  (syntax-parse now-stx
    [(head #:escape_as escape:identifier (~and b (tag::block . _)))
     (values #`(let/ec esc
                 (define escape esc)
                 (define-static-info-syntax escape . #,(get-continuation-static-infos))
                 (finish-esc-try #,stx (head b)))
             #'())]
    [(_ (tag::block g ...))
     (define-values (rev-gs state)
       (for/fold ([rev-gs null]
                  [state #hasheq()])
                 ([g (in-list (syntax->list #'(g ...)))])
         (syntax-parse g
           #:datum-literals (group)
           [(group #:initially . _)
            #:when (hash-ref state 'initially #f)
            (raise-syntax-error #f "duplicate `~initially` clause" stx g)]
           [(group #:initially . _)
            #:when (or (pair? rev-gs) (positive? (hash-count state)))
            (raise-syntax-error #f "`~initially` clause must appear at the start of the body" stx g)]
           [(group #:initially (tag::block body ...))
            (values rev-gs (hash-set state 'initially #'(rhombus-body-at tag body ...)))]
           [(group #:initially term ...+)
            (values rev-gs (hash-set state 'initially #`(rhombus-expression (#,group-tag term ...))))]
           [(group #:initially . _)
            (raise-syntax-error #f "expected block or expression after `~initially`" stx g)]
           [(group #:finally . _)
            #:when (hash-ref state 'finally #f)
            (raise-syntax-error #f "duplicate `~finally` clause" stx g)]
           [(group #:finally (tag::block body ...))
            (values rev-gs (hash-set state 'finally #'(rhombus-body-at tag body ...)))]
           [(group #:finally term ...+)
            (values rev-gs (hash-set state 'finally #`(rhombus-expression (#,group-tag term ...))))]
           [(group #:finally . _)
            (raise-syntax-error #f "expected block or expression after `~finally`" stx g)]
           [(group #:catch . _)
            #:when (hash-ref state 'handler #f)
            (raise-syntax-error #f "duplicate `~catch` clause" stx g)]
           [(group #:catch . _)
            #:when (hash-ref state 'finally #f)
            (raise-syntax-error #f "`~catch` not allowed after `~finally`" stx g)]
           [(group #:catch (_::alts b ...))
            (define-values (b-parseds rhss)
              (for/lists (b-parseds rhss)
                  ([b (in-list (syntax->list #'(b ...)))])
                (syntax-parse b
                  #:datum-literals (group)
                  [(_::block (group bind ...+ (~and rhs (_::block . _))))
                   #:with b::binding #`(#,group-tag bind ...)
                   (values #'b.parsed #'rhs)]
                  [_ (raise-syntax-error #f
                                         "expected a binding and block for `~catch` alternative"
                                         stx
                                         b)])))
            (define handler (build-try-handler b-parseds rhss))
            (values rev-gs (hash-set state 'handler handler))]
           [(group #:catch bind ...+ (~and rhs (_::block . _)))
            #:with b::binding #`(#,group-tag bind ...)
            (define handler (build-try-handler (list #'b.parsed) (list #'rhs)))
            (values rev-gs (hash-set state 'handler handler))]
           [(group #:catch . _)
            (raise-syntax-error #f "expected alternatives or a binding and block after `~catch`" stx g)]
           [_
            (when (or (hash-ref state 'handler #f)
                      (hash-ref state 'finally #f))
              (raise-syntax-error #f "expression or definition not allowed after `~catch` or `~finally`" stx g))
            (values (cons g rev-gs) state)])))
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
       (values body #'()))]
    [_
     (raise-syntax-error #f
                         "expected a block, optionally preceded by an `~escape_as` and an identifier"
                         stx)]))

(define-syntax try
  (expression-transformer
   (lambda (stx)
     (parse-try stx stx))))

(define-syntax finish-esc-try
  (lambda (stx)
    (syntax-parse stx
      [(_ stx now-stx)
       (define-values (e tail) (parse-try #'stx #'now-stx))
       e])))

(define-syntax throw
  (expression-prefix-operator
   #f
   '((default . weaker))
   'automatic
   (lambda (form1 op-stx)
     #`(raise #,form1))))

(void (set-primitive-who! 'call-with-composable-continuation 'Continuation.capture))
(define-syntax capture
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier (tag::block g ...))
        (values #'(call-with-composable-continuation
                   (lambda (id)
                     (define-static-info-syntax id
                       #:getter get-continuation-static-infos)
                     (rhombus-body-at tag g ...)))
                #'())]
       [(_ tag-expr ...+ id:identifier (tag::block g ...))
        (values #`(call-with-composable-continuation
                   (lambda (id)
                     (define-static-info-syntax id
                       #:getter get-continuation-static-infos)
                     (rhombus-body-at tag g ...))
                   (rhombus-expression (#,group-tag tag-expr ...)))
                #'())]))))

(void (set-primitive-who! 'call-in-continuation 'Continuation.in))
(define-syntax in
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ cont-expr ... (tag::block g ...))
        (values #`(call-in-continuation
                   (rhombus-expression (#,group-tag cont-expr ...))
                   (lambda () (rhombus-body-at tag g ...)))
                #'())]))))

(void (set-primitive-who! 'call-with-continuation-prompt 'Continuation.prompt))
(define-syntax prompt
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ tag-expr ... (tag::block g ...))
        (define-values (rev-gs state)
          (for/fold ([rev-gs null]
                     [state #hasheq()])
                    ([g (in-list (syntax->list #'(g ...)))])
            (syntax-parse g
              #:datum-literals (group)
              [(group #:catch . _)
               #:when (hash-ref state 'handler #f)
               (raise-syntax-error #f "duplicate `~catch` clause" stx g)]
              [(group #:catch (_::alts b ...))
               (define-values (argss arg-parsedss rhss)
                 (for/lists (argss arg-parsedss rhss)
                            ([b (in-list (syntax->list #'(b ...)))])
                   (syntax-parse b
                     #:datum-literals (group)
                     [(_::block (group (_::parens arg::binding ...) (~and rhs (_::block . _))))
                      (values #'(arg ...) #'(arg.parsed ...) #'rhs)]
                     [(_::block (group bind ...+ (~and rhs (_::block . _))))
                      #:with arg::binding #`(#,group-tag bind ...)
                      (values #'(arg) #'(arg.parsed) #'rhs)]
                     [_ (raise-syntax-error #f
                                            "expected a binding and block for `~catch` alternative"
                                            stx
                                            b)])))
               (define falses (map (lambda (f) #f) argss))
               (define falses-stx (datum->syntax #f (map (lambda (f) #'#f) argss)))
               (define-values (handler arity)
                 (build-case-function no-adjustments '()
                                      #'prompt_handler falses falses
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
                                      g))
               (values rev-gs (hash-set state 'handler handler))]
              [(group #:catch (_::parens arg::binding ...) (~and rhs (_::block . _)))
               (define falses-stx (datum->syntax #f (map (lambda (a) #'#f) (syntax->list #'(arg ...)))))
               (define-values (handler arity)
                 (build-function no-adjustments '()
                                 #'prompt_handler #f #f
                                 falses-stx #'(arg ...) #'(arg.parsed ...) falses-stx
                                 #'#f #'#f
                                 #'#f #'#f
                                 #f #f
                                 #'rhs
                                 g))
               (values rev-gs (hash-set state 'handler handler))]
              [(group #:catch bind ...+ (~and rhs (_::block . _)))
               #:with arg::binding #`(#,group-tag bind ...)
               (define-values (handler arity)
                 (build-function no-adjustments '()
                                 #'prompt_handler #f #f
                                 #'(#f) #'(arg) #'(arg.parsed) #'(#f)
                                 #'#f #'#f
                                 #'#f #'#f
                                 #f #f
                                 #'rhs
                                 g))
               (values rev-gs (hash-set state 'handler handler))]
              [(group #:catch (_::block entry))
               #:with (~var e (:entry-point no-adjustments)) #'entry
               (values rev-gs (hash-set state 'handler #'e.parsed))]
              [(group #:catch . _)
               (raise-syntax-error #f "expected alternatives or a binding and block after `~catch`" stx g)]
              [_
               (when (hash-ref state 'handler #f)
                 (raise-syntax-error #f "expression or definition not allowed after `~catch`" stx g))
               (values (cons g rev-gs) state)])))
        (values #`(call-with-continuation-prompt
                   (lambda () (rhombus-body-at tag #,@(reverse rev-gs)))
                   #,(if (null? (syntax-e #'(tag-expr ...)))
                         #'(default-continuation-prompt-tag)
                         #`(rhombus-expression (#,group-tag tag-expr ...)))
                   #,@(let ([handler (hash-ref state 'handler #f)])
                        (if handler
                            (list handler)
                            null)))
                #'())]))))

(define-syntax barrier
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block g ...))
        (values #`(call-with-continuation-barrier
                   (lambda () (rhombus-body-at tag g ...)))
                #'())]))))

(define/arity (Continuation.escape
               #:tag [prompt-tag (default-continuation-prompt-tag)]
               . vals)
  #:primitive (abort-current-continuation)
  (apply abort-current-continuation prompt-tag vals))

(define Continuation.PromptTag.default
  (default-continuation-prompt-tag))

(define/arity (Continuation.PromptTag.make [name-in #f])
  (define name
    (cond
      [(not name-in) #f]
      [(symbol? name-in) name-in]
      [(string? name-in) (string->symbol name-in)]
      [else (raise-annotation-failure who
                                      name-in
                                      "maybe(ReadableString || Symbol)")]))
  (if name
      (make-continuation-prompt-tag name)
      (make-continuation-prompt-tag)))

(define-for-syntax (build-try-handler b-parseds rhss)
  (for/foldr ([next #'raise])
             ([b-parsed (in-list b-parseds)]
              [rhs (in-list rhss)])
    (syntax-parse b-parsed
      [arg-parsed::binding-form
       #:with arg-impl::binding-impl #'(arg-parsed.infoer-id () arg-parsed.data)
       #:with arg::binding-info #'arg-impl.info
       #:with (tag g ...) rhs
       #`(let ()
           (arg.oncer-id arg.data)
           (lambda (arg-id)
             (arg.matcher-id arg-id arg.data
                             if/blocked
                             (let ()
                               (arg.committer-id arg-id arg.evidence-ids arg.data)
                               (arg.binder-id arg-id arg.evidence-ids arg.data)
                               (define-static-info-syntax/maybe arg.bind-id arg.bind-static-info ...)
                               ...
                               (rhombus-body-at tag g ...))
                             (#,next arg-id))))])))

(define (always-true x) #t)

(define-syntax with_mark
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ lhs ...+ _::equal rhs ...+ (tag::block g ...))
        (check-multiple-equals stx)
        (values #`(with-continuation-mark
                    (rhombus-expression (#,group-tag lhs ...))
                    (rhombus-expression (#,group-tag rhs ...))
                    (rhombus-body-at tag g ...))
                #'())]))))

(define/arity (Continuation.call_with_immediate_mark
               key proc
               #:default [default #f])
  #:primitive (call-with-immediate-continuation-mark)
  (call-with-immediate-continuation-mark key proc default))
