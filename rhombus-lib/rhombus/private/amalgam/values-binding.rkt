#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt"
                     "origin.rkt")
         "parse.rkt"
         "binding.rkt"
         "values-key.rkt"
         "parens.rkt")

(provide (for-syntax parse-values-binding))

(define-for-syntax (parse-values-binding stx allow-one-multi?)
  (syntax-parse stx
    [(form-id (~and p (_::parens val::binding ...)) . tail)
     (define val-parseds (syntax->list #'(val.parsed ...)))
     (values (syntax-parse val-parseds
               ;; expose the original infoer, important for certain optimizations
               [(val-parsed)
                (unless allow-one-multi? ; allow one multiple-value binding, like `#%parens`
                  (syntax-parse #'val-parsed
                    [_::binding-form (void)]))
                #'val-parsed]
               [(val-b::binding-form ...)
                (transfer-origins
                 val-parseds
                 (binding-values-form
                  (list #'form-id #'p)
                  (length (syntax->list #'(val ...)))
                  #'values-infoer
                  #'[(val-b.infoer-id val-b.data) ...]))])
             #'tail)]))

(define-syntax (values-infoer stx)
  (syntax-parse stx
    [(_ (up-static-infos ...) [(val-infoer val-data) ...])
     #:with (val-impl::binding-impl ...) #'((val-infoer up-static-infos val-data) ...)
     #:with (val::binding-info ...) #'(val-impl.info ...)
     (binding-info (string-append
                    "values("
                    (apply string-append
                           (for/list ([annot-str-stx (in-list (syntax->list #'(val.annotation-str ...)))]
                                      [i (in-naturals 0)])
                             (string-append
                              (if (zero? i) "" ", ")
                              (syntax-e annot-str-stx))))
                    ")")
                   #'(val.name-id ...)
                   ;; if we reach here, arity must not be 1
                   #'((#%values (val.static-infos ...)))
                   #'((val.bind-id val.bind-uses val.bind-static-info ...) ... ...)
                   #'values-oncer
                   #'values-matcher
                   #'(val.evidence-ids ...)
                   #'values-committer
                   #'values-binder
                   #'(val ...))]))

(define-syntax (values-oncer stx)
  (syntax-parse stx
    [(_ (val::binding-info ...))
     #'(begin
         (val.oncer-id val.data)
         ...)]))

(define-syntax (values-matcher stx)
  (syntax-parse stx
    [(_ (arg-id ...) (val::binding-info ...) IF success failure)
     (for/foldr ([success #'success])
                ([arg-id (in-list (syntax->list #'(arg-id ...)))]
                 [val-matcher-id (in-list (syntax->list #'(val.matcher-id ...)))]
                 [val-data (in-list (syntax->list #'(val.data ...)))])
       #`(#,val-matcher-id #,arg-id #,val-data IF #,success failure))]))

(define-syntax (values-committer stx)
  (syntax-parse stx
    [(_ (arg-id ...) (evidence-ids ...) (val::binding-info ...))
     #'(begin
         (val.committer-id arg-id evidence-ids val.data)
         ...)]))

(define-syntax (values-binder stx)
  (syntax-parse stx
    [(_ (arg-id ...) (evidence-ids ...) (val::binding-info ...))
     #'(begin
         (val.binder-id arg-id evidence-ids val.data)
         ...)]))
