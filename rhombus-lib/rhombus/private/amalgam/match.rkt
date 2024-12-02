#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "else-clause.rkt"
         "realm.rkt"
         "parens.rkt"
         (submod "quasiquote.rkt" for-match)
         (only-in "literal.rkt" literal-infoer)
         "if-blocked.rkt"
         "nested-bindings.rkt"
         "static-info.rkt"
         "../version-case.rkt")

;; TEMP approximate `case/equal-always`
(meta-if-version-at-least
 "8.11.1.8"
 (require (only-in racket/case case/equal-always))
 (require (only-in racket/base [case case/equal-always])))

(provide match
         matches)

(begin-for-syntax
  (define-syntax-class :pattern-clause
    #:attributes ([bind 1] rhs)
    #:datum-literals (group)
    (pattern (_::block (group bind ...
                              (~and rhs (_::block . _)))))))

(define-syntax match
  (expression-transformer
   (lambda (stx)
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
          #`(#,group-tag in ...)
          #'(b ...)
          #'(b.parsed ...)
          #'(clause.rhs ...)
          ;; `fallback-k` is called with the remaining non-literal patterns
          (lambda (val-id statinfos bs b-parseds rhss)
            (handle-normal-match
             val-id statinfos bs b-parseds rhss
             #'e.parsed)))
         #'())]
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...))
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (define in-expr #`(#,group-tag in ...))
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
             ;; `fallback-k` is called with the remaining non-literal patterns
             (lambda (val-id statinfos bs b-parseds rhss)
               (handle-normal-match
                val-id statinfos bs b-parseds rhss
                #`(match-fallthrough 'form-id #,val-id #,(syntax-srcloc (respan stx))))))))
         #'())]
       [(form-id in ...+ (block-tag::block))
        (values
         (relocate+reraw
          (respan stx)
          #`(match-fallthrough 'form-id (rhombus-expression (#,group-tag in ...)) #,(syntax-srcloc (respan stx))))
         #'())]
       [(form-id in ...+ (_::alts clause ...))
        (for ([c (in-list (syntax->list #'(clause ...)))])
          (syntax-parse c
            [_::pattern-clause (void)]
            [_ (raise-syntax-error #f
                                   "expected a pattern followed by a result block"
                                   c)]))]))))

(define-for-syntax (handle-normal-match val-id statinfos bs b-parseds rhss else-expr)
  (for/foldr ([next else-expr])
             ([b (in-list bs)]
              [b-parsed (in-list b-parseds)]
              [rhs (in-list rhss)])
    (syntax-parse b-parsed
      [b-parsed::binding-form
       #:with b-impl::binding-impl #`(b-parsed.infoer-id #,statinfos b-parsed.data)
       #:with b-info::binding-info #'b-impl.info
       ;; use `((lambda ....) ....)` to keep textual order
       #`((lambda (try-next)
            (nested-bindings
             match
             try-next
             #f ; failure
             (#,val-id b-info #,b #f)
             (begin
               (b-info.committer-id #,val-id b-info.evidence-ids b-info.data)
               (b-info.binder-id #,val-id b-info.evidence-ids b-info.data)
               (define-static-info-syntax/maybe b-info.bind-id b-info.bind-static-info ...)
               ...
               (rhombus-body-expression #,rhs))))
          (lambda () #,next))])))

(define-for-syntax (handle-literal-case-dispatch stx in-expr
                                                 bs-stx b-parseds-stx rhss-stx
                                                 fallback-k)
  (syntax-parse in-expr
    [e::expression
     (define statinfos (extract-static-infos #'e.parsed))
     (define (split-at lst idx)
       (cond
         [(eqv? idx 0)
          (values '() lst)]
         [else
          (define-values (l-lst r-lst) (split-at (cdr lst) (sub1 idx)))
          (values (cons (car lst) l-lst) r-lst)]))
     (define bs (syntax->list bs-stx))
     (define b-parseds (syntax->list b-parseds-stx))
     (define rhss (syntax->list rhss-stx))
     ;; index at which the initial literal segment ends, if any
     (define maybe-idx
       (for/fold ([maybe-idx #f])
                 ([parsed (in-list b-parseds)]
                  [idx (in-naturals 1)])
         #:break (syntax-parse parsed
                   [b::binding-form
                    (not (free-identifier=? #'b.infoer-id #'literal-infoer))])
         idx))
     (relocate+reraw
      (respan stx)
      #`(let ([val #,(discard-static-infos #'e.parsed)])
          #,(cond
              [maybe-idx
               (define rst-bs (list-tail bs maybe-idx))
               (define-values (lit-parseds rst-parseds) (split-at b-parseds maybe-idx))
               (define-values (lit-rhss rst-rhss) (split-at rhss maybe-idx))
               #`(case/equal-always val
                                    #,@(for/list ([parsed (in-list lit-parseds)]
                                                  [rhs (in-list lit-rhss)])
                                         (syntax-parse parsed
                                           [b::binding-form
                                            #:with ([datum _] ...) #'b.data
                                            #`[(datum ...) (rhombus-body-expression #,rhs)]]))
                                    [else #,(fallback-k #'val statinfos rst-bs rst-parseds rst-rhss)])]
              [else
               (fallback-k #'val statinfos bs b-parseds rhss)])))]))

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

(define-syntax matches
  (expression-infix-operator
   `((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       [(op . tail)
        #:with (~var t (:infix-op+binding+tail #'matches)) #`(#,group-tag . tail)
        (values
         (syntax-parse #'t.parsed
           [b::binding-form
            #:with b-impl::binding-impl #'(b.infoer-id () b.data)
            #:with b-info::binding-info #'b-impl.info
            #`(let ([val #,form])
                (b-info.matcher-id val b-info.data
                                   if/blocked #t #f))])
         #'t.tail)]))
   'none))

;; for precedence
(define-binding-syntax matches
  (binding-infix-operator
   `((default . stronger))
   'macro
   (lambda (form tail) (error "should not get here"))
   'none))
