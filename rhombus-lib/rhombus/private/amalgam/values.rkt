#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "srcloc.rkt")
         "provide.rkt"
         "binding.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "reducer.rkt"
         "static-info.rkt"
         "function-arity-key.rkt"
         "values-key.rkt"
         (submod "define-arity.rkt" for-info)
         "parens.rkt"
         "op-literal.rkt"
         "var-decl.rkt"
         "simple-call.rkt"
         "parse.rkt"
         "compound-repetition.rkt"
         "if-blocked.rkt"
         "binding-failure.rkt")

(provide (for-spaces (rhombus/bind
                      rhombus/annot
                      rhombus/reducer
                      rhombus/statinfo)
                     values)
         (for-spaces (rhombus/reducer)
                     (rename-out [values fold]))
         (for-spaces (#f
                      rhombus/repet)
                     (rename-out [rhombus-values values]))
         (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [call-with-values call_with_values])))

(module+ for-parse
  (provide (for-syntax :values-id-annot
                       :values-id-bind)))

(define-syntax rhombus-values
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head (_::parens g ...) . tail)
        #:when (simple-call? stx)
        (syntax-parse #'(g ...)
          [(e::expression ...)
           (define es (syntax->list #'(e.parsed ...)))
           (cond
             [(= 1 (length es))
              (values (wrap-static-info*
                       #`(values #,(discard-static-infos (car es)))
                       (extract-static-infos (car es)))
                      #'tail)]
             [else
              (values (wrap-static-info
                       #`(values #,@(map discard-static-infos es))
                       #'#%values
                       (datum->syntax #f (map extract-static-infos es)))
                      #'tail)])])]
       [(head . tail)
        (values (relocate-id #'head #'values)
                #'tail)]))))

(define-repetition-syntax rhombus-values
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head (_::parens g ...) . tail)
        #:when (simple-call? stx #:repetition? #t)
        (syntax-parse #'(g ...)
          [(e::repetition ...)
           (define es (syntax->list #'(e.parsed ...)))
           (define sis
             (for/list ([e (in-list es)])
               (syntax-parse e
                 [rep::repetition-info
                  #'rep.element-static-infos])))
           (values
            (build-compound-repetition
             #'head
             es
             (lambda new-content
               (values #`(values #,@new-content)
                       (if (= 1 (length sis))
                           (car sis)
                           #`((#%values #,sis))))))
            #'tail)])]
       [(head . tail)
        (values (identifier-repetition-use (relocate-id #'head #'values))
                #'tail)]))))

(define-binding-syntax values
  (binding-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as a pattern (except as a non-nested"
                                           " pattern by forms that specifically recognize it)")
                            #'head)]))))

(define-annotation-syntax values
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as an annotation (except as a non-nested"
                                           " annotation by forms that specifically recognize it)")
                            #'head)]))))

(begin-for-syntax
  (define-syntax-rule (define-values-id-class class in-space quote)
    (define-syntax-class class
      #:attributes (name)
      #:description "the literal `values`"
      #:opaque
      (pattern ::name
               #:when (free-identifier=? (in-space #'name)
                                         (quote values)))))
  (define-values-id-class :values-id-annot in-annotation-space annot-quote)
  (define-values-id-class :values-id-bind in-binding-space bind-quote))

(begin-for-syntax
  (define-splicing-syntax-class :accum
    #:description "accumulator with optional annotation"
    #:attributes (id e static-infos pre-defn defns checks)
    (pattern d::var-decl
      #:with b::binding #'(group d.bind ...)
      #:with b-parsed::binding-form #'b.parsed
      #:with b-impl::binding-impl #`(b-parsed.infoer-id () b-parsed.data)
      #:with b-info::binding-info #'b-impl.info
      #:with ((bind-id bind-use . bind-static-infos) ...) #'b-info.bind-infos
      #:with e #'d.default
      #:with id ((make-syntax-introducer) (datum->syntax #f (syntax-e #'b-info.name-id)))
      #:with static-infos #'b-info.static-infos
      #:with (evidence-id ...) (let loop ([evidence #'b-info.evidence-ids]
                                          [accum null])
                                 (cond
                                   [(identifier? evidence) (cons evidence accum)]
                                   [(syntax? evidence)
                                    (for/fold ([accum accum]) ([evidence (in-list (or (syntax->list evidence) null))])
                                      (loop evidence accum))]
                                   [else accum]))
      #:with matcher-id ((make-syntax-introducer) #'matcher)
      #:with pre-defn #'(define (matcher-id id)
                          (b-info.oncer-id b-info.data)
                          (b-info.matcher-id id
                                             b-info.data
                                             if/flattened
                                             (begin)
                                             (values-binding-failure who id 'b-info.annotation-str))
                          (values evidence-id ...))
      #:attr defns #`((define-values (evidence-id ...) (matcher-id id))
                      (b-info.committer-id id b-info.evidence-ids b-info.data)
                      (b-info.binder-id id b-info.evidence-ids b-info.data)
                      (define-static-info-syntax/maybe bind-id . bind-static-infos)
                      ...)
      #:attr checks #`((matcher-id id)
                       (void)))))

(define-reducer-syntax values
  (reducer-transformer
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (group)
       [(form-id (_::parens (group accum::accum) ...) . tail)
        (values
         (reducer/no-break #:pre-defns #'[(define who 'form-id) accum.pre-defn ...]
                           #'build-values-check-result
                           #'([accum.id accum.e]
                              ...)
                           #:pre-clause #'build-values-defns
                           #'build-values-next
                           (let* ([siss #'(accum.static-infos
                                          ...)]
                                  [siss-l (syntax->list siss)])
                             (if (= 1 (length siss-l))
                                 (car siss-l)
                                 #`((#%values #,siss))))
                           #'([accum.id accum.defns accum.checks]
                              ...))
         #'tail)]))))

(define-syntax (build-values-check-result stx)
  (syntax-parse stx
    [(_ ([id defns (check ...)] ...) e)
     #'(let-values ([(id ...) e])
         check
         ...
         ...
         (values id ...))]))

(define-syntax (build-values-defns stx)
  (syntax-parse stx
    [(_ ([id (defn ...) check] ...))
     #'(begin
         defn
         ...
         ...)]))

(define-syntax (build-values-next stx)
  (syntax-parse stx
    [(_ ([id defns checks] ...) e)
     #'(let-values ([(id ...) e])
         (values id ...))]))

(define-static-info-syntax values
  (#%function-arity -1)
  . #,(indirect-get-function-static-infos))

(define-static-info-syntax call-with-values
  (#%function-arity 4)
  . #,(indirect-get-function-static-infos))

(define (values-binding-failure who val annot)
  (raise-binding-failure who "value" val annot))
