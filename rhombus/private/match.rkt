#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     syntax/parse/pre
                     "srcloc.rkt"
                     "annotation-string.rkt"
                     "tag.rkt"
                     (submod "entry-point-adjustment.rkt" for-struct))
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "else-clause.rkt"
         (submod "function-parse.rkt" for-build)
         "realm.rkt"
         "parens.rkt"
         (submod "quasiquote.rkt" for-match)
         (only-in "literal.rkt" literal-infoer)
         "version-case.rkt")
;; TEMP approximate `case/equal-always`
(meta-if-version-at-least
 "8.11.1.8"
 (require (only-in racket/case case/equal-always))
 (require (only-in racket/base [case case/equal-always])))

(provide match)

(begin-for-syntax
  (define-syntax-class :pattern-clause
    #:attributes ([bind 1] rhs)
    #:datum-literals (group)
    (pattern (_::block (group bind ...
                              (~and rhs (_::block . _)))))))

(define-syntax match
  (expression-transformer
   (lambda (stx)
     (define ((make-fallback-k else else-parsed else-rhs) val bs b-parseds rhss)
       (define (make-consts const)
         (cons const (map (lambda (x) const) bs)))
       (define (make-consts-stx const)
         (datum->syntax #f (make-consts const)))
       (define falses (make-consts #f))
       (define falses-stx (make-consts-stx #'#f))
       (define-values (proc arity)
         (build-case-function no-adjustments
                              #'match
                              #f #f
                              (make-consts-stx #'(#f))
                              #`(#,@(map list bs) (#,else))
                              #`(#,@(map list b-parseds) (#,else-parsed))
                              falses-stx falses-stx
                              falses-stx falses-stx
                              falses falses
                              #`(#,@rhss #,else-rhs)
                              stx))
       #`(#,proc #,val))
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...
                          e::else-clause))
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (values
         (handle-literal-case-dispatch
          stx
          #'(rhombus-expression (group in ...))
          #'(b ...)
          #'(b.parsed ...)
          #'(clause.rhs ...)
          ;; fallback-k is called with the remaining non-literal patterns
          (make-fallback-k #'ignored
                           (binding-form #'else-infoer #'(#t ignored))
                           #'(parsed #:rhombus/expr e.parsed)))
         #'())]
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...))
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (define in-expr #'(rhombus-expression (group in ...)))
        (define b-parseds-stx #'(b.parsed ...))
        (define rhss-stx #'(clause.rhs ...))
        (values
         (handle-syntax-parse-dispatch
          stx #'form-id in-expr b-parseds-stx rhss-stx
          ;; thunk is called if any `b.parsed` is not a syntax pattern
          (lambda ()
            (handle-literal-case-dispatch
             stx in-expr
             #'(b ...) b-parseds-stx rhss-stx
             ;; fallback-k is called with the remaining non-literal patterns
             (make-fallback-k #'unmatched
                              (binding-form #'else-infoer #'(#t unmatched))
                              #`(parsed
                                 #:rhombus/expr
                                 (match-fallthrough 'form-id unmatched #,(syntax-srcloc (respan stx))))))))
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

(define-for-syntax (handle-literal-case-dispatch stx in-expr
                                                 bs-stx b-parseds-stx rhss-stx
                                                 fallback-k)
  (define bs (syntax->list bs-stx))
  (define b-parseds (syntax->list b-parseds-stx))
  (define rhss (syntax->list rhss-stx))
  (define maybe-idx
    (for/fold ([maybe-idx #f])
              ([parsed (in-list b-parseds)]
               [idx (in-naturals 0)])
      #:break (syntax-parse parsed
                [b::binding-form
                 (not (free-identifier=? #'b.infoer-id #'literal-infoer))])
      (add1 idx)))
  (cond
    [maybe-idx
     (define rst-bs (list-tail bs maybe-idx))
     (define-values (lit-parseds rst-parseds) (split-at b-parseds maybe-idx))
     (define-values (lit-rhss rst-rhss) (split-at rhss maybe-idx))
     (relocate+reraw
      (respan stx)
      #`(let ([val #,in-expr])
          (case/equal-always val
            #,@(for/list ([parsed (in-list lit-parseds)]
                          [rhs (in-list lit-rhss)])
                 (syntax-parse parsed
                   [b::binding-form
                    #`[b.data (rhombus-body-expression #,rhs)]]))
            [else #,(fallback-k #'val rst-bs rst-parseds rst-rhss)])))]
    [else (relocate+reraw
           (respan stx)
           (fallback-k in-expr bs b-parseds rhss))]))

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
