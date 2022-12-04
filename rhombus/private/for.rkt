#lang racket/base
(require (for-syntax racket/base
                     racket/syntax-srcloc
                     syntax/srcloc
                     syntax/parse
                     "tag.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "reducer.rkt"
         (submod "reducer.rkt" for-class)
         "for-clause.rkt"
         "static-info.rkt"
         "ref-result-key.rkt"
         "parse.rkt"
         "parens.rkt"
         (rename-in "values.rkt"
                    [values rhombus-values]))

(provide (rename-out [rhombus-for for])
         each
         keep_when
         skip_when
         break_when
         final_when)

(define-syntax rhombus-for
  (expression-transformer
   #'for
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (block group)
       [(form-id ((~and block-tag block) body ...+ (group #:into red ...)))
        (values #'(rhombus-expression
                   (group form-id red ... (block-tag body ...)))
                #'())]
       [(form-id (block body ...+))
        (values #`(for (#:splice (for-clause-step #,stx [(finish (begin))] body ...))
                    (finish))
                #'())]
       [(form-id red ... (block body ...+))
        #:with g-tag group-tag
        #:with redr::reducer #'(g-tag red ...)
        #:with f::reducer-form #'redr.parsed
        (values (wrap-static-info*
                 #`(f.wrapper
                    (for/fold f.binds (#:splice (for-clause-step #,stx [(finish f.body-wrapper)] body ...))
                      (finish)))
                 #'f.static-infos)
                #'())]))))

(require (for-syntax racket/pretty))

(define-splicing-for-clause-syntax for-clause-step
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (group block parens)
      #:literals (rhombus-values)
      [(_ orig [finish] . bodys)
       ;; initialize state
       #`(#:splice (for-clause-step orig [finish () () (void) (void)]
                                    . bodys))]
      [(_ orig [(finish (body-wrap ...)) rev-clauses rev-bodys matcher binder])
       (when (null? (syntax-e #'rev-bodys))
         (raise-syntax-error #f
                             "empty body (after any keyword clauses, such as `~each`)"
                             #'orig))
       #`(#,@(reverse (syntax->list #'rev-clauses))
          #:do [matcher
                binder
                (define (finish)
                  (body-wrap
                   ...
                   (rhombus-body
                    . #,(reverse (syntax->list #'rev-bodys)))))])]
      [(_ orig (~and state [finish rev-clauses rev-bodys matcher binder])
          body0
          . bodys)
       #:when (for-clause? #'body0)
       (cond
         [(pair? (syntax-e #'rev-bodys))
          ;; emit accumulated body and clauses before starting more clauses
          #`(#,@(reverse (syntax->list #'rev-clauses))
             #:do (matcher
                   binder
                   (rhombus-body-sequence
                    . #,(reverse (syntax->list #'rev-bodys))))
             #:splice (for-clause-step orig
                                       [finish () () (void) (void)]
                                       body0 . bodys))]
         [(pair? (syntax-e #'rev-clauses)) ; assert: empty rev-bodys
          ;; emit clauses before starting a new group
          #`(#,@(reverse (syntax->list #'rev-clauses))
             #:do [matcher binder]
             #:splice (for-clause-step orig
                                       [finish () () (void) (void)]
                                       body0 . bodys))]
         [else
          (syntax-parse #'body0
            #:datum-literals (group block parens)
            #:literals (prim-for-clause)
            [(group prim-for-clause #:each any ...+ (~and rhs-block (block body ...)))
             ;; parse binding as binding group
             #`(#:splice (for-clause-step
                          orig
                          #,(build-binding-clause/values #'orig
                                                         #'state
                                                         #`((#,group-tag any ...))
                                                         #'rhs-block)
                          . bodys))]
            [(group prim-for-clause #:each (block (group any ...+ (~and rhs-block (block body ...)))
                                                  ...))
             ;; parse binding as binding group
             #`(#:splice (for-clause-step
                          orig
                          #,(build-binding-clause*/values #'orig
                                                          #'state
                                                          (syntax->list #`(((#,group-tag any ...)) ...))
                                                          (syntax->list #'(rhs-block ...)))
                          . bodys))]
            [(group prim-for-clause (~and kw (~or #:keep_when #:skip_when #:break_when #:final_when))
                    expr ...)
             #:with new-kw (case (syntax-e #'kw)
                       [(#:keep_when) (datum->syntax #'kw '#:when #'kw #'kw)]
                       [(#:skip_when) (datum->syntax #'kw '#:unless #'kw #'kw)]
                       [(#:break_when) (datum->syntax #'kw '#:break #'kw #'kw)]
                       [(#:final_when) (datum->syntax #'kw '#:final #'kw #'kw)]
                       [else #'kw])
             ;; assert: empty rev-bodys and rev-clauses
             (when (null? (syntax-e #'(expr ...)))
               (raise-syntax-error #f
                                   (format "missing expression after `~~~a`"
                                           (keyword->string (syntax-e #'kw)))
                                   #'orig
                                   #'kw))
             #`(new-kw (rhombus-expression (#,group-tag expr ...))
                       #:splice (for-clause-step orig state . bodys))]
            [body0::for-clause
             #:with f::for-clause-form #'body0.parsed
             #`(#:splice (for-clause-step orig state f.parsed ... . bodys))])])]
      [(_ orig [finish rev-clauses rev-bodys matcher binder]
          body0
          . bodys)
       #`(#:splice (for-clause-step
                    orig
                    [finish
                     rev-clauses
                     (body0 . rev-bodys)
                     matcher
                     binder]
                    . bodys))])))

(define-for-syntax (build-binding-clause/values orig-stx
                                                state-stx
                                                bindings-stx
                                                rhs-block-stx)
  (build-binding-clause orig-stx
                        state-stx
                        (syntax-parse bindings-stx
                          #:datum-literals (group parens block)
                          #:literals (rhombus-values)
                          [((group (~optional rhombus-values) (parens g ...)))
                           #'(g ...)]
                          [else bindings-stx])
                        rhs-block-stx))

(define-for-syntax (build-binding-clause orig-stx
                                         state-stx
                                         bindings-stx
                                         rhs-block-stx)
  (define lhs-parsed-stxes (for/list ([binding-stx (in-list (syntax->list bindings-stx))])
                             (syntax-parse binding-stx
                               [lhs::binding #'lhs.parsed]
                               [_ (raise-syntax-error #f
                                                      "expected a binding"
                                                      orig-stx
                                                      binding-stx)])))
  (syntax-parse lhs-parsed-stxes
    [(lhs-e::binding-form ...)
     #:with rhs (rhombus-local-expand (enforest-expression-block rhs-block-stx))
     #:with static-infos (or (syntax-local-static-info #'rhs #'#%ref-result)
                             #'())
     #:with (lhs-impl::binding-impl ...) #'((lhs-e.infoer-id static-infos lhs-e.data)...)
     #:with (lhs-i::binding-info ...) #'(lhs-impl.info ...)
     #:with (form-id . _) orig-stx
     #:with (tmp-id ...) (generate-temporaries #'(lhs-i.name-id ...))
     (define seq-ctr (syntax-local-static-info #'rhs #'#%sequence-constructor))
     (syntax-parse state-stx
       [[finish rev-clauses rev-bodys matcher binder]
        #`[finish
           ([(tmp-id ...) #,(cond
                              [seq-ctr #`(#,seq-ctr rhs)]
                              [else #'rhs])]
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
               (lhs-i.binder-id tmp-id lhs-i.data)
               (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
               ...)
             ...)]])]))

(define-for-syntax (build-binding-clause*/values orig-stx
                                                 state-stx
                                                 bindings-stxs
                                                 rhs-block-stxs)
    (cond
      [(null? bindings-stxs) state-stx]
      [else
       (define new-state-stx (build-binding-clause/values orig-stx
                                                          state-stx
                                                          (car bindings-stxs)
                                                          (car rhs-block-stxs)))
       (build-binding-clause*/values orig-stx
                                     new-state-stx
                                     (cdr bindings-stxs)
                                     (cdr rhs-block-stxs))]))

(define-syntax-rule (void-result e)
  (begin
    e
    (void)))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding-str)
  (raise-binding-failure who "element" val binding-str))

;; ----------------------------------------

;; To recognize all primitive forms:
(define-syntax prim-for-clause
  (for-clause-transformer
   (lambda (stx)
     (raise-syntax-error #f "should not try to expand" stx))))

(define-syntax each
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group block)
       [(form-id bind ...+ (~and rhs (_::block . _)))
        #`[(#,group-tag prim-for-clause #:each bind ... rhs)]]
       [(form-id (~and blk
                       (_::block (group bind ...+ (_::block . _))
                                 ...)))
        #`[(#,group-tag prim-for-clause #:each blk)]]
       [_
        (raise-syntax-error #f
                            "needs binding followed by value block, or it needs a block of bindings with blocks"
                            #'orig
                            #'body0)]))))

(define-syntax keep_when
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals ()
       [(form-id expr ...+)
        #`[(#,group-tag prim-for-clause #:keep_when expr ...)]]))))

(define-syntax skip_when
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals ()
       [(form-id expr ...+)
        #`[(#,group-tag prim-for-clause #:skip_when expr ...)]]))))

(define-syntax break_when
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals ()
       [(form-id expr ...+)
        #`[(#,group-tag prim-for-clause #:break_when expr ...)]]))))

(define-syntax final_when
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals ()
       [(form-id expr ...+)
        #`[(#,group-tag prim-for-clause #:final_when expr ...)]]))))

