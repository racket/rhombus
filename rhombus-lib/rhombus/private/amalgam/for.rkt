#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "tag.rkt"
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "for-clause-expand.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "reducer.rkt"
         "for-clause.rkt"
         "static-info.rkt"
         "index-result-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "parse.rkt"
         "parens.rkt"
         "is-static.rkt"
         "forwarding-sequence.rkt"
         "syntax-parameter.rkt"
         "if-blocked.rkt"
         (only-in "for-clause-primitive.rkt"
                  each)
         (submod "membership-testable.rkt" in-operator))

(provide (rename-out [rhombus-for for]))

(begin-for-syntax
  (define-syntax-class :maybe-ends-each
    #:attributes (each red-parsed)
    #:datum-literals (group)
    (pattern ((_::parens (~and g (~or (group bind ...+ _::in _ ...+)
                                      (group bind ...+ (_::block . _))))
                         ...))
             #:with each #`(#,group-tag each (block g ...))
             #:attr red-parsed #f)
    (pattern ()
             #:attr each #f
             #:attr red-parsed #f)
    (pattern (red ...)
             #:with (~var redr (:infix-op+reducer+tail #'#%call)) (no-srcloc #`(#,group-tag red ...))
             #:attr each (syntax-parse #'redr.tail
                           #:datum-literals (group)
                           [((_::parens (~and g (~or (group bind ...+ _::in _ ...+)
                                                     (group bind ...+ (_::block . _))))
                                        ...))
                            (no-srcloc #`(#,group-tag each (block g ...)))]
                           [()
                            #f])
             #:with red-parsed #'redr.parsed)))

(define-syntax rhombus-for
  (expression-transformer
   (lambda (stx)
     (define static? (is-static-context/tail? stx))
     (define-values (red-parsed body)
       (syntax-parse stx
         #:datum-literals (group)
         [(_ pre-t ... (_::block body ... (group #:into red ...)))
          #:cut
          #:with pre::maybe-ends-each #'(pre-t ...)
          #:do [(when (attribute pre.red-parsed)
                  (raise-syntax-error #f
                                      "cannot have both `~into` and reducer terms before block"
                                      stx))]
          #:with redr::reducer #`(#,group-tag red ...)
          (values #'redr.parsed
                  #'((~? pre.each) body ...))]
         [(_ pre-t ... (_::block body ...))
          #:cut
          #:with pre::maybe-ends-each #'(pre-t ...)
          (values (attribute pre.red-parsed)
                  #'((~? pre.each) body ...))]))
     (values
      (cond
        [(not red-parsed)
         (relocate+reraw
          (respan stx)
          #`(for (#:splice (for-clause-step #,stx
                                            #,static?
                                            [(void-result [])]
                                            . #,body))
              (void)))]
        [else
         (syntax-parse red-parsed
           [f::reducer-form
            (wrap-static-info*
             (relocate+reraw
              (respan stx)
              #`(f.wrapper
                 f.data
                 (for/fold f.binds
                           (#:splice (for-clause-step #,stx
                                                      #,static?
                                                      [(f.body-wrapper f.data)
                                                       #,@(if (syntax-e #'f.pre-clause-former)
                                                              (list #'(f.pre-clause-former f.data))
                                                              '())]
                                                      . #,body))
                   #,@(if (syntax-e #'f.break-whener)
                          #`(#:break (f.break-whener f.data))
                          null)
                   #,@(if (syntax-e #'f.final-whener)
                          #`(#:final (f.final-whener f.data))
                          null)
                   (f.finisher f.data))))
             #'f.static-infos)])])
      #'()))))

(define-syntax (void-result stx)
  (syntax-parse stx
    [(_ _ e) #'e]))

(define-splicing-for-clause-syntax for-clause-step
  (lambda (stx)
    (with-syntax-error-respan
      (syntax-parse stx
        #:datum-literals (group)
        [(_ orig static? [finish (~optional pre-clause-form)] . bodys)
         ;; initialize state
         #`(#:splice (for-clause-step orig static?
                                      [finish () () (void) (~? pre-clause-form (void)) #hasheq()]
                                      . bodys))]
        [(_ orig static? [(body-wrapper data) rev-clauses rev-bodys matcher binder stx-params])
         (when (null? (syntax-e #'rev-bodys))
           (raise-syntax-error #f
                               "empty body (after any clauses such as `each`)"
                               (respan #'orig)))
         #`(#,@(reverse (syntax->list #'rev-clauses))
            #:do [matcher
                  binder
                  (body-wrapper
                   data
                   (with-syntax-parameters stx-params
                     (rhombus-body
                      . #,(reverse (syntax->list #'rev-bodys)))))])]
        [(_ orig static? (~and state [finish rev-clauses rev-bodys matcher binder stx-params])
            body0
            . bodys)
         #:when (for-clause? #'body0)
         (cond
           [(pair? (syntax-e #'rev-clauses))
            ;; emit clauses and bind before processing a (potentially non-empty) body
            #`(#,@(reverse (syntax->list #'rev-clauses))
               #:do [matcher
                     binder]
               #:splice (for-clause-step orig static?
                                         [finish () rev-bodys (void) (void) stx-params]
                                         body0 . bodys))]
           [(pair? (syntax-e #'rev-bodys)) ; assert: empty rev-clauses
            ;; emit accumulated body with forward-sequence expansion
            (expand-forwarding-sequence
             #`((rhombus-body-sequence
                 . #,(reverse (syntax->list #'rev-bodys))))
             #'(body0 . bodys)
             #'#hasheq()
             syntax-local-splicing-for-clause-introduce
             ;; continue when some expr+defns are ready:
             (lambda (exprs+defns state)
               #`(#:do (#,@exprs+defns)
                  ;; `for-clause-forwarding-step` will use
                  ;; `expand-forwarding-sequence-continue`
                  ;; and eventually get back to `for-clause-step` mode:
                  #:splice (for-clause-forwarding-step
                            orig static? finish
                            #,state)))
             ;; continue when no more exprs and defns:
             (lambda (exprs+defns bodys stx-params)
               #`(#:do (#,@exprs+defns)
                  #:splice (for-clause-step orig static?
                                            [finish () () (void) (void) #,stx-params]
                                            . #,bodys))))]
           [else
            (define parsed
              (with-continuation-mark syntax-parameters-key #'stx-params
                                      (syntax-parse #'body0
                                        [body0::for-clause #'body0.parsed])))
            (syntax-parse parsed
              [(#:each bind-gss rhs-blks)
               #`(#:splice (for-clause-step
                            orig static?
                            #,(build-binding-clause* #'orig
                                                     #'state
                                                     (syntax->list #'bind-gss)
                                                     (syntax->list #'rhs-blks)
                                                     (syntax-e #'static?))
                            . bodys))]
              [((~and kw (~or* #:when #:unless #:break #:final))
                rhs)
               #`(kw
                  (with-syntax-parameters stx-params rhs)
                  #:splice (for-clause-step orig static? state . bodys))]
              [(#:let bind-gs rhs)
               (define-values (pre-defns evidence post-defns)
                 (expand-let-clause #'stx-params #'bind-gs #'rhs))
               #`(#:do (#,@pre-defns)
                  #:when #,evidence
                  #:do (#,@post-defns)
                  #:splice (for-clause-step orig static? state . bodys))]
              [(#:splice new ...)
               #`(#:splice (for-clause-step orig static? state new ... . bodys))])])]
        [(_ orig static? [finish rev-clauses rev-bodys matcher binder stx-params]
            body0
            . bodys)
         #`(#:splice (for-clause-step
                      orig static?
                      [finish
                       rev-clauses
                       (body0 . rev-bodys)
                       matcher
                       binder
                       stx-params]
                      . bodys))]))))

;; trampoline back into `expand-forwarding-sequence-continue`, eventually
;; returning to the `for-clause-step` trampoline:
(define-splicing-for-clause-syntax for-clause-forwarding-step
  (lambda (stx)
    (with-syntax-error-respan
      (syntax-parse stx
        [(_ orig static? finish state)
         (expand-forwarding-sequence-continue
          #'state
          syntax-local-splicing-for-clause-introduce
          ;; continue when another expr or defn is ready:
          (lambda (exprs+defns state)
            #`(#:do (#,@exprs+defns)
               #:splice (for-clause-forwarding-step
                         orig static? finish
                         #,state)))
          ;; continue when no more exprs and defns:
          (lambda (exprs+defns bodys stx-params)
            #`(#:do (#,@exprs+defns)
               #:splice (for-clause-step orig static?
                                         [finish () () (void) (void) #,stx-params]
                                         . #,bodys))))]))))

(define-for-syntax (build-binding-clause orig-stx
                                         state-stx
                                         bind-gs-stx
                                         rhs-blk-stx
                                         static?)
  (syntax-parse state-stx
    [[finish rev-clauses rev-bodys matcher binder stx-params]
     #:do [(define lhs-parsed-stxes (for/list ([bind-g (in-list (syntax->list bind-gs-stx))])
                                      (syntax-parse bind-g
                                        [lhs::binding #'lhs.parsed]
                                        [_ (raise-syntax-error #f
                                                               "expected a binding"
                                                               (respan orig-stx)
                                                               (respan bind-g))])))]
     #:with (lhs-e::binding-form ...) lhs-parsed-stxes
     #:with rhs (with-continuation-mark syntax-parameters-key #'stx-params
                  (rhombus-local-expand (enforest-expression-block rhs-blk-stx)))
     #:with (static-infos ...) (normalize-static-infos/values
                                (length lhs-parsed-stxes)
                                (or (syntax-local-static-info #'rhs #'#%sequence-element)
                                    (syntax-local-static-info #'rhs #'#%index-result)
                                    #'()))
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id static-infos lhs-e.data) ...)
     #:with (lhs-i::binding-info ...) #'(lhs-impl.info ...)
     #:with (form-id . _) orig-stx
     #:with (tmp-id ...) (for/list ([name-id (in-list (syntax->list #'(lhs-i.name-id ...)))])
                           ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id) name-id)))
     (define seq-ctr (syntax-local-static-info #'rhs #'#%sequence-constructor))
     (when (and static? (not seq-ctr))
       (raise-syntax-error #f
                           (string-append "no specific iteration implementation available" statically-str)
                           (respan orig-stx)
                           (respan rhs-blk-stx)))
     #`[finish
        ([(tmp-id ...) #,(relocate ; this helps debugging info
                          rhs-blk-stx
                          (add-with-syntax-parameters
                           #'stx-params
                           (cond
                             [(identifier? seq-ctr)
                              (if (syntax-local-value* seq-ctr expression-prefix-operator-ref)
                                  (unwrap-static-infos
                                   (with-continuation-mark syntax-parameters-key #'stx-params
                                     (rhombus-local-expand
                                      #`(rhombus-expression (group #,seq-ctr (parens (group (parsed #:rhombus/expr rhs))))))))
                                  #`(#,seq-ctr rhs))]
                             [else
                              (unwrap-static-infos #'rhs)])))]
         . rev-clauses)
        ()
        (begin
          matcher
          (lhs-i.oncer-id lhs-i.data)
          ...
          (lhs-i.matcher-id tmp-id
                            lhs-i.data
                            if/flattened
                            (begin)
                            (rhs-binding-failure 'form-id tmp-id 'lhs-i.annotation-str))
          ...)
        (begin
          binder
          (lhs-i.committer-id tmp-id lhs-i.evidence-ids lhs-i.data)
          ...
          (lhs-i.binder-id tmp-id lhs-i.evidence-ids lhs-i.data)
          ...
          (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
          ... ...)
        stx-params]]))

(define-for-syntax (build-binding-clause* orig-stx
                                          state-stx
                                          bind-gs-stxs
                                          rhs-blk-stxs
                                          static?)
  (for/fold ([state-stx state-stx])
            ([bind-gs-stx (in-list bind-gs-stxs)]
             [rhs-blk-stx (in-list rhs-blk-stxs)])
    (build-binding-clause orig-stx
                          state-stx
                          bind-gs-stx
                          rhs-blk-stx
                          static?)))

(define (rhs-binding-failure who val binding-str)
  (raise-binding-failure who "element" val binding-str))

;; NOTE since we need a "flat" structure for clauses in Racket `for`,
;; the expansion needs to pack evidences, if any, into a vector and
;; unpack it only after the `#:when` test.  If there isn't any
;; evidence in the bindings, we use a simple boolean instead.
(define-for-syntax (expand-let-clause stx-params bind-gs-stx rhs-stx)
  (define (flatten-tree t)
    (cond
      [(identifier? t) (list t)]
      [else (apply append (map flatten-tree (syntax->list t)))]))
  (syntax-parse bind-gs-stx
    [(lhs::binding ...)
     #:with (lhs-e::binding-form ...) #'(lhs.parsed ...)
     #:with rhs (with-continuation-mark syntax-parameters-key stx-params
                  (rhombus-local-expand (enforest-expression-block rhs-stx)))
     #:with (static-infos ...) (normalize-static-infos/values
                                (length (syntax->list bind-gs-stx))
                                (extract-static-infos #'rhs))
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id static-infos lhs-e.data) ...)
     #:with (lhs-i::binding-info ...) #'(lhs-impl.info ...)
     #:with (tmp-id ...) (generate-temporaries #'(lhs-i.name-id ...))
     #:with (lhs-i-evidence-id ...) (flatten-tree #'(lhs-i.evidence-ids ...))
     (define need-evidence? (not (null? (syntax-e #'(lhs-i-evidence-id ...)))))
     (values
      #`((lhs-i.oncer-id lhs-i.data)
         ...
         (define-values (tmp-id ...) (let-values ([(lhs-i.name-id ...) #,(discard-static-infos #'rhs)])
                                       (values lhs-i.name-id ...)))
         (define evidence
           #,(for/foldr ([success (if need-evidence?
                                      #'(vector lhs-i-evidence-id ...)
                                      #'#t)])
                        ([lhs-i-matcher-id (in-list (syntax->list #'(lhs-i.matcher-id ...)))]
                         [tmp-id (in-list (syntax->list #'(tmp-id ...)))]
                         [lhs-i-data (in-list (syntax->list #'(lhs-i.data ...)))])
               #`(#,lhs-i-matcher-id #,tmp-id #,lhs-i-data if/blocked
                                     #,success
                                     #f))))
      #'evidence
      #`(#,@(if need-evidence?
                (list #`(define-values (lhs-i-evidence-id ...) (vector->values evidence)))
                '())
         (lhs-i.committer-id tmp-id lhs-i.evidence-ids lhs-i.data)
         ...
         (lhs-i.binder-id tmp-id lhs-i.evidence-ids lhs-i.data)
         ...
         (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
         ... ...))]))

(define-for-syntax (add-with-syntax-parameters stx-params rhs)
  (if (eqv? (hash-count (syntax-e stx-params)) 0)
      rhs
      ;; FIXME this can hide an otherwise immediate sequence form
      #`(with-syntax-parameters #,stx-params #,rhs)))

;; ----------------------------------------

;; not exported, but referenced by `:maybe-ends-each` so that
;; reducer parsing terminates appropriately
(define-reducer-syntax #%call
  (reducer-infix-operator
   #f
   `((default . stronger))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))
