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
         (only-in "entry-point.rkt" no-adjustments))

(provide match)

(module+ for-match-id
  (provide (for-syntax :match)))

(begin-for-syntax
  (define-syntax-class :pattern-clause
    #:attributes ([bind 1] rhs)
    #:datum-literals (group)
    (pattern (_::block (group bind ...
                              (~and rhs (_::block . _))))))

  (define (falses l-stx)
    (datum->syntax #f (map (lambda (x) #f) (cons 'b (syntax->list l-stx)))))

  (define (l1falses l-stx)
    (datum->syntax #f (map (lambda (x) '(#f)) (cons 'b (syntax->list l-stx)))))
  
  (define-syntax-class :match
    #:description "match form"
    #:opaque
    #:attributes ()
    (pattern name::name
             #:when (free-identifier=? #'name.name (expr-quote match)))))

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
                               #'(clause.rhs ... (parsed e.parsed))
                               #'form-id #'alts-tag))
        (values
         #`(#,proc (rhombus-expression (group in ...)))
         #'())]
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...))
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (define-values (proc arity)
          (build-case-function no-adjustments
                               #'match #'#f
                               (l1falses #'(b ...))
                               #'((b) ... (unmatched))
                               #`((b.parsed) ... (#,(binding-form
                                                     #'else-infoer
                                                     #'(#f unmatched))))
                               (falses #'(b ...)) (falses #'(b ...))
                               (falses #'(b ...)) (falses #'(b ...))
                               (falses #'(b ...))
                               #`(clause.rhs ... (parsed
                                                  (match-fallthrough 'form-id unmatched #,(syntax-srcloc (respan stx)))))
                               #'form-id #'alts-tag))
        (values
         #`(#,proc (rhombus-expression (group in ...)))
         #'())]
       [(form-id in ...+ (block-tag::block))
        (values
         #`((match-fallthrough 'form-id (rhombus-expression (group in ...)) #,(syntax-srcloc (respan stx)))
            (rhombus-expression (group in ...)))
         #'())]
       [(form-id in ...+ (_::alts clause ...))
        (for ([c (in-list (syntax->list #'(clause ...)))])
          (syntax-parse c
            [(c::pattern-clause ...) (void)]
            [_ (raise-syntax-error #f
                                   "expected a pattern followed by a result block"
                                   c)]))]))))

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
