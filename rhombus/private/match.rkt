#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "annotation-string.rkt"
                     "tag.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         (submod "function.rkt" for-build)
         "realm.rkt"
         "parens.rkt"
         (only-in "entry-point.rkt" no-adjustments))

(provide match)

(begin-for-syntax
  (define-syntax-class :pattern-clause
    #:datum-literals (block group)
    (pattern (block (group bind ...
                           (~and rhs (block . _))))))

  (define (falses l-stx)
    (datum->syntax #f (map (lambda (x) #f) (cons 'b (syntax->list l-stx)))))

  (define (l1falses l-stx)
    (datum->syntax #f (map (lambda (x) '(#f)) (cons 'b (syntax->list l-stx))))))

(define-syntax match
  (expression-transformer
   #'match
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id in ...+ (alts-tag::alts
                          clause::pattern-clause
                          ...
                          (block (group #:else
                                        (~and else-rhs (block . _)))))
                 . tail)
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag clause.bind ...) ...))
        (values
         #`(#,(build-case-function no-adjustments
                                   #'match
                                   (l1falses #'(b ...))
                                   #'((b) ... (ignored))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'else-infoer
                                                         #'(#t ignored))))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...))
                                   #'(clause.rhs ... else-rhs)
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ...+ (alts-tag::alts
                          (block (group bind ...
                                        (~and rhs (block . _))))
                          ...)
                 . tail)
        #:with (b::binding ...) (no-srcloc* #`((#,group-tag bind ...) ...))
        (values
         #`(#,(build-case-function no-adjustments
                                   #'match
                                   (l1falses #'(b ...))
                                   #'((b) ... (unmatched))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'else-infoer
                                                         #'(#f unmatched))))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...)) (falses #'(b ...))
                                   (falses #'(b ...))
                                   #`(rhs ... (parsed
                                               (match-fallthrough 'form-id unmatched #,(syntax-srcloc (respan stx)))))
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ...+ (block-tag::block) . tail)
        (values
         #`((match-fallthrough 'form-id (rhombus-expression (group in ...)) #,(syntax-srcloc (respan stx)))
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ...+ (_::alts clause ...) . tail)
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
                   #'else-binder
                   #'(ok? bind-id))]))

(define-syntax (else-matcher stx)
  (syntax-parse stx
    [(_ arg-id (ok? bind-id) IF success fail)
     #'(IF ok?
           success
           fail)]))

(define-syntax (else-binder stx)
  (syntax-parse stx
    [(_ arg-id (ok? bind-id))
     #'(define bind-id arg-id)]))
