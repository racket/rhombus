#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         (submod "function.rkt" for-build))

(provide match)

(begin-for-syntax
  (define-syntax-class :pattern-clause
    #:datum-literals (block group)
    (pattern (block (group bind ...
                           (~and rhs (block . _)))))))

(define-syntax match
  (expression-transformer
   #'match
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id in ... ((~and alts-tag alts)
                         clause::pattern-clause
                         ...
                         (block (group #:else
                                       (~and else-rhs (block . _)))))
                 . tail)
        #:with (b::binding ...) #'((group clause.bind ...) ...)
        (values
         #`(#,(build-case-function #'match
                                   #'((b) ... (ignored))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'ignored
                                                         #'else-matcher
                                                         #'else-binder
                                                         #'(#t ignored))))
                                   #'(clause.rhs ... else-rhs)
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ... ((~and alts-tag alts)
                         (block (group bind ...
                                       (~and rhs (block . _))))
                         ...)
                 . tail)
        #:with (b::binding ...) #'((group bind ...) ...)
        (values
         #`(#,(build-case-function #'match
                                   #'((b) ... (unmatched))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'unmatched
                                                         #'else-matcher
                                                         #'else-binder
                                                         #'(#f unmatched))))
                                   #`(rhs ... (parsed
                                               (match-fallthrough 'form-id unmatched #,(syntax-srcloc (respan stx)))))
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ... ((~and block-tag block)) . tail)
        (values
         #`((match-fallthrough 'form-id (rhombus-expression (group in ...)) #,(syntax-srcloc (respan stx)))
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ... (alts clause ...) . tail)
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
    (format "~a: no matching case" who)
    (current-continuation-marks)
    (if loc
        (list loc)
        null))))

(define (match-fallthrough who v loc)
  (raise-srcloc-error who v loc))

(define-syntax (else-matcher stx)
  (syntax-parse stx
    [(_ arg-id (ok? bind-id) IF success fail)
     #'(IF ok?
           success
           fail)]))

(define-syntax (else-binder stx)
  (syntax-parse stx
    [(_ arg-id (_ bind-id))
     #'(define bind-id arg-id)]))
