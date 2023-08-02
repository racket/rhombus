#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "srcloc.rkt"
                     "annotation-string.rkt"
                     "tag.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "else-clause.rkt"
         (submod "function-parse.rkt" for-build)
         "realm.rkt"
         "parens.rkt"
         (only-in "entry-point.rkt" no-adjustments)
         (submod "quasiquote.rkt" for-match)
         (only-in "literal.rkt" literal-infoer))

(provide match)

(begin-for-syntax
  (define-syntax-class :pattern-clause
    #:attributes ([bind 1] rhs)
    #:datum-literals (group)
    (pattern (_::block (group bind ...
                              (~and rhs (_::block . _))))))

  (define (falses l-stx)
    (datum->syntax #f (map (lambda (x) #f) (cons 'b (syntax->list l-stx)))))

  (define (l1falses l-stx)
    (datum->syntax #f (map (lambda (x) '(#f)) (cons 'b (syntax->list l-stx))))))

(define-syntax match
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...
                          e::else-clause))
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (define in-expr #'(rhombus-expression (group in ...)))
        (values
         (handle-literal-case-dispatch
          stx
          #'form-id
          in-expr
          #'(b.parsed ...)
          #'(clause.rhs ...)
          #'e.parsed
          ;; thunk is called if any `b.parsed` is not a literal pattern
          (lambda ()
            (define-values (proc arity)
              (build-case-function no-adjustments
                                   #'match #'#f
                                   (l1falses #'(b ...))
                                   #'((b) ... (ignored))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'else-infoer
                                                         #'(#t ignored))))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...))
                                   #'(clause.rhs ... (parsed #:rhombus/expr e.parsed))
                                   stx))
            (relocate+reraw
             (respan stx)
             #`(#,proc #,in-expr))))
         #'())]
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...))
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (define in-expr #'(rhombus-expression (group in ...)))
        (values
         (handle-syntax-parse-or-literal-case-dispatch
          stx
          #'form-id
          in-expr
          #'(b.parsed ...)
          #'(clause.rhs ...)
          #f
          ;; thunk is called if any `b.parsed` is not a syntax pattern
          (lambda ()
            (define-values (proc arity)
              (build-case-function no-adjustments
                                   #'match #'#f
                                   (l1falses #'(b ...))
                                   #'((b) ... (unmatched))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'else-infoer
                                                         #'(#t unmatched))))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...))
                                   #`(clause.rhs ... (parsed
                                                      #:rhombus/expr
                                                      (match-fallthrough 'form-id unmatched #,(syntax-srcloc (respan stx)))))
                                   stx))
            (relocate+reraw
             (respan stx)
             #`(#,proc #,in-expr))))
         #'())]
       [(form-id in ...+ (block-tag::block))
        (values
         (relocate+reraw
          (respan stx)
          #`((match-fallthrough 'form-id (rhombus-expression (group in ...)) #,(syntax-srcloc (respan stx)))
             (rhombus-expression (group in ...))))
         #'())]
       [(form-id in ...+ (_::alts clause ...))
        (for ([c (in-list (syntax->list #'(clause ...)))])
          (syntax-parse c
            [_::pattern-clause (void)]
            [_ (raise-syntax-error #f
                                   "expected a pattern followed by a result block"
                                   c)]))]))))

(define-for-syntax (handle-literal-case-dispatch stx form-id in-expr binds-stx rhss-stx else-parsed thunk)
  (define binds (syntax->list binds-stx))
  (cond
    [(for/and ([bind-stx (in-list binds)])
       (syntax-parse bind-stx
         [b::binding-form
          (free-identifier=? #'b.infoer-id #'literal-infoer)]))
     (relocate+reraw
      (respan stx)
      #`(let ([val #,in-expr])
          (case val
            #,@(for/list ([bind-stx (in-list binds)]
                          [rhs (in-list (syntax->list rhss-stx))])
                 (syntax-parse bind-stx
                   [b::binding-form
                    #`[b.data (rhombus-body-expression #,rhs)]]))
            [else #,(or else-parsed
                        #`(match-fallthrough '#,form-id val #,(syntax-srcloc (respan stx))))])))]
    [else (thunk)]))

(define-for-syntax (handle-syntax-parse-or-literal-case-dispatch stx form-id in-expr binds-stx rhss-stx else-parsed thunk)
  (handle-syntax-parse-dispatch
   stx form-id in-expr binds-stx rhss-stx 
   (lambda ()
     (handle-literal-case-dispatch
      stx form-id in-expr binds-stx rhss-stx else-parsed
      thunk))))

(struct exn:fail:contract:srcloc exn:fail:contract (srclocs)
  #:property prop:exn:srclocs (lambda (exn) (exn:fail:contract:srcloc-srclocs exn)))

(define (raise-srcloc-error who v loc)
  (raise
   (exn:fail:contract:srcloc
    (error-message->adjusted-string
     who
     rhombus-realm
     "no matching case"
     rhombus-realm)
    (current-continuation-marks)
    (if loc
        (list loc)
        null))))

(define (match-fallthrough who v loc)
  (raise-srcloc-error who v loc))

(define-syntax (else-infoer stx)
  (syntax-parse stx
    [(_ static-infos (ok? bind-id))
     (binding-info annotation-any-string
                   #'bind-id
                   #'static-infos
                   #'()
                   #'else-matcher
                   #'else-committer
                   #'else-binder
                   #'(ok? bind-id))]))

(define-syntax (else-matcher stx)
  (syntax-parse stx
    [(_ arg-id (ok? bind-id) IF success fail)
     #'(IF ok?
           success
           fail)]))

(define-syntax (else-committer stx)
  (syntax-parse stx
    [(_ arg-id (ok? bind-id))
     #'(begin)]))

(define-syntax (else-binder stx)
  (syntax-parse stx
    [(_ arg-id (ok? bind-id))
     #'(define bind-id arg-id)]))
