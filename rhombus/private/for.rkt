#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
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
         "values-key.rkt"
         "parse.rkt"
         "parens.rkt"
         (rename-in "values.rkt"
                    [values rhombus-values])
         "is-static.rkt"
         "forwarding-sequence.rkt"
         "syntax-parameter.rkt")

(provide (rename-out [rhombus-for for])
         (for-space rhombus/for_clause
                    each
                    keep_when
                    skip_when
                    break_when
                    final_when))

(begin-for-syntax
  (define-syntax-class :maybe_ends_each
    #:attributes (each red-parsed)
    #:datum-literals (group)
    (pattern ((_::parens (~and g (group bind ...+ (_::block . _))) ...))
             #:with each #`(#,group-tag each (block g ...))
             #:attr red-parsed #f)
    (pattern ()
             #:attr each #f
             #:attr red-parsed #f)
    (pattern (red ...)
             #:with (~var redr (:infix-op+reducer+tail #'#%call)) #`(#,group-tag red ...)
             #:attr each (syntax-parse #'redr.tail
                           #:datum-literals (group)
                           [((_::parens (~and g (group bind ...+ (_::block . _))) ...))
                            #`(#,group-tag each (block g ...))]
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
         [(_ pre_t ... (_::block body ...+ (group #:into red ...)))
          #:cut
          #:with pre::maybe_ends_each #'(pre_t ...)
          #:do [(when (attribute pre.red-parsed)
                  (raise-syntax-error #f
                                      "cannot have both `~into` and reducer terms before block"
                                      stx))]
          #:with redr::reducer #`(#,group-tag red ...)
          (values #'redr.parsed
                  #'((~? pre.each) body ...))]
         [(_ pre_t ... (_::block body ...+))
          #:cut
          #:with pre::maybe_ends_each #'(pre_t ...)
          (values (attribute pre.red-parsed)
                  #'((~? pre.each) body ...))]))
     (values
      (cond
        [(not red-parsed)
         (relocate+reraw
          (respan stx)
          #`(for (#:splice (for-clause-step #,stx
                                            #,static?
                                            [(begin (void))]
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
                                                      [(f.body-wrapper f.data)]
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

(define-splicing-for-clause-syntax for-clause-step
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (group block parens)
      [(_ orig static? [finish] . bodys)
       ;; initialize state
       #`(#:splice (for-clause-step orig static? [finish () () (void) (void) #hasheq()]
                                    . bodys))]
      [(_ orig static? [(body-wrapper data) rev-clauses rev-bodys matcher binder stx-params])
       (when (null? (syntax-e #'rev-bodys))
         (raise-syntax-error #f
                             "empty body (after any clauses such as `each`)"
                             (respan #'orig)))
       #`(#,@(reverse (map (add-clause-stx-params #'stx-params) (syntax->list #'rev-clauses)))
          #:do [matcher
                binder
                (body-wrapper
                 data
                 (with-syntax-parameters
                  stx-params
                   (rhombus-body
                    . #,(reverse (syntax->list #'rev-bodys)))))])]
      [(_ orig static? (~and state [finish rev-clauses rev-bodys matcher binder stx-params])
          body0
          . bodys)
       #:when (for-clause? #'body0)
       (cond
         [(pair? (syntax-e #'rev-clauses))
          ;; emit clauses and bind before processing a (potentially non-empty) body
          #`(#,@(reverse (map (add-clause-stx-params #'stx-params) (syntax->list #'rev-clauses)))
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
                ;; `for-clause-forwaring-step` will  use
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
          (with-continuation-mark
           syntax-parameters-key #'stx-params
           (syntax-parse #'body0
             #:datum-literals (group block parens)
             #:literals (prim-for-clause)
             [(group prim-for-clause #:each any ...+ rhs-blk)
              ;; parse binding as binding group
              #`(#:splice (for-clause-step
                           orig static?
                           #,(build-binding-clause/values #'orig
                                                          #'state
                                                          #`((#,group-tag any ...))
                                                          #'rhs-blk
                                                          (syntax-e #'static?))
                           . bodys))]
             [(group prim-for-clause #:each (block (group any ...+ rhs-blk)
                                                   ...))
              ;; parse binding as binding group
              #`(#:splice (for-clause-step
                           orig static?
                           #,(build-binding-clause*/values #'orig
                                                           #'state
                                                           (syntax->list #`(((#,group-tag any ...)) ...))
                                                           (syntax->list #'(rhs-blk ...))
                                                           (syntax-e #'static?))
                           . bodys))]
             [(group prim-for-clause (~and kw (~or #:keep_when #:skip_when #:break_when #:final_when))
                     rhs)
              #:with new-kw (case (syntax-e #'kw)
                              [(#:keep_when) (datum->syntax #'kw '#:when #'kw #'kw)]
                              [(#:skip_when) (datum->syntax #'kw '#:unless #'kw #'kw)]
                              [(#:break_when) (datum->syntax #'kw '#:break #'kw #'kw)]
                              [(#:final_when) (datum->syntax #'kw '#:final #'kw #'kw)]
                              [else #'kw])
              #`(new-kw rhs
                        #:splice (for-clause-step orig static? state . bodys))]
             [body0::for-clause
              #:with f::for-clause-form #'body0.parsed
              #`(#:splice (for-clause-step orig static? state f.parsed ... . bodys))]))])]
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
                    . bodys))])))

;; trampoline back into `expand-forwarding-sequence-continue`, eventually
;; returning to the `for-clause-step` trampoline:
(define-splicing-for-clause-syntax for-clause-forwarding-step
  (lambda (stx)
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
                                       . #,bodys))))])))

(begin-for-syntax
  (define-syntax-class :values-id
    #:attributes (name)
    #:description "the literal `values`"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-binding-space #'name)
                                       (bind-quote rhombus-values)))))

(define-for-syntax (build-binding-clause/values orig-stx
                                                state-stx
                                                bindings-stx
                                                rhs-block-stx
                                                static?)
  (build-binding-clause orig-stx
                        state-stx
                        (syntax-parse bindings-stx
                          #:datum-literals (group parens block)
                          [((group (~optional _::values-id) (parens g ...)))
                           #'(g ...)]
                          [else bindings-stx])
                        rhs-block-stx
                        static?))

(define-for-syntax (build-binding-clause orig-stx
                                         state-stx
                                         bindings-stx
                                         rhs-blk-stx
                                         static?)
  (define lhs-parsed-stxes (for/list ([binding-stx (in-list (syntax->list bindings-stx))])
                             (syntax-parse binding-stx
                               [lhs::binding #'lhs.parsed]
                               [_ (raise-syntax-error #f
                                                      "expected a binding"
                                                      (respan orig-stx)
                                                      (respan binding-stx))])))
  (syntax-parse lhs-parsed-stxes
    [(lhs-e::binding-form ...)
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-blk-stx))
     #:with (static-infos ...) (cond
                                 [(or (syntax-local-static-info #'rhs #'#%sequence-element)
                                      (syntax-local-static-info #'rhs #'#%index-result))
                                  => (lambda (infos)
                                       (define number-of-vals (length (syntax->list #'(lhs-e ...))))
                                       (define infoss
                                         (syntax-parse infos
                                           #:literals (#%values)
                                           [((#%values (si ...))) (syntax->list #'(si ...))]
                                           [_ (list infos)]))
                                       (if (= (length infoss) number-of-vals)
                                           infoss
                                           (for/list ([idx (in-range number-of-vals)])
                                             #'())))]
                                 [else (for/list ([lhs-e (in-list (syntax->list #'(lhs-e ...)))])
                                         #'())])
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id static-infos lhs-e.data)...)
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
     (syntax-parse state-stx
       [[finish rev-clauses rev-bodys matcher binder stx-params]
        #`[finish
           ([(tmp-id ...) #,(cond
                              [(identifier? seq-ctr)
                               (if (syntax-local-value* seq-ctr expression-prefix-operator-ref)
                                   (unwrap-static-infos
                                    (rhombus-local-expand
                                     #`(rhombus-expression (group #,seq-ctr (parens (group (parsed #:rhombus/expr rhs)))))))
                                   #`(#,seq-ctr rhs))]
                              [else (unwrap-static-infos #'rhs)])]
            . rev-clauses)
           ()
           (begin
             matcher
             (lhs-i.matcher-id tmp-id
                               lhs-i.data
                               flattened-if
                               (void)
                               (rhs-binding-failure 'form-id tmp-id 'lhs-i.annotation-str))
             ...)
           (begin
             binder
             (begin
               (lhs-i.committer-id tmp-id lhs-i.data)
               (lhs-i.binder-id tmp-id lhs-i.data)
               (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
               ...)
             ...)
           stx-params]])]))

(define-for-syntax (build-binding-clause*/values orig-stx
                                                 state-stx
                                                 bindings-stxs
                                                 rhs-blk-stxs
                                                 static?)
    (cond
      [(null? bindings-stxs) state-stx]
      [else
       (define new-state-stx (build-binding-clause/values orig-stx
                                                          state-stx
                                                          (car bindings-stxs)
                                                          (car rhs-blk-stxs)
                                                          static?))
       (build-binding-clause*/values orig-stx
                                     new-state-stx
                                     (cdr bindings-stxs)
                                     (cdr rhs-blk-stxs)
                                     static?)]))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding-str)
  (raise-binding-failure who "element" val binding-str))

(define-for-syntax ((add-clause-stx-params stx-params) clause)
  (cond
    [(zero? (hash-count (syntax-e stx-params))) clause]
    [else
     (syntax-parse clause
       [[bind rhs]
        ;; since `for` uses `local-expand` to recognize optimized patterns,
        ;; and since `with-syntax-parameters` also uses `local-expand-expression`,
        ;; this `with-syntax-parameters` wrapper doesn't interfere with optimization
        #`[bind (with-syntax-parameters #,stx-params rhs)]])]))

;; ----------------------------------------

;; To recognize all primitive forms:
(define-syntax prim-for-clause
  (for-clause-transformer
   (lambda (stx)
     (raise-syntax-error #f "should not try to expand" (respan stx)))))

(begin-for-syntax
  ;; Like `:var-decl`, but we don't allow `=` here
  (define-splicing-syntax-class :each-decl
    #:datum-literals (group block)
    #:attributes ([bind 1] blk)
    (pattern (~seq bind ...+ (~and rhs (_::block . _)))
             #:attr blk #'rhs)))

(define-for-clause-syntax each
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group block)
       [(form-id d::each-decl)
        #`[(#,group-tag prim-for-clause #:each d.bind ... d.blk)]]
       [(form-id (tag::block (group d::each-decl) ...))
        #`[(#,group-tag prim-for-clause #:each (tag
                                                (group d.bind ... d.blk)
                                                ...))]]
       [_
        (raise-syntax-error #f
                            "needs a binding followed by a block, or it needs a block of bindings (each with a block)"
                            (respan stx))]))))

(define-for-syntax (parse-when stx kw)
  (syntax-parse stx
    #:datum-literals ()
    [(form-id (tag::block g ...))
     #`[(#,group-tag prim-for-clause #,kw (rhombus-body-at tag g ...))]]
    [(form-id expr ...+)
     #`[(#,group-tag prim-for-clause #,kw (rhombus-expression (#,group-tag expr ...)))]]
    [(form-id)
     (raise-syntax-error #f
                         "missing expression"
                         (respan #'stx))]))

(define-for-clause-syntax keep_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:keep_when))))

(define-for-clause-syntax skip_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:skip_when))))

(define-for-clause-syntax break_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:break_when))))

(define-for-clause-syntax final_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:final_when))))

;; not exported, but referenced by `:maybe_ends_each` so that
;; reducer parsing terminates appropriately
(define-reducer-syntax #%call
  (reducer-infix-operator
   (reducer-quote #%call)
   `((default . stronger))
   'macro
   (lambda (stx) (error "should not get here"))
   'none))
