#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (only-in shrubbery/print
                              shrubbery-syntax->string)
                     "annotation-string.rkt")
         "binding.rkt"
         "unquote-binding.rkt"
         (submod "unquote-binding-primitive.rkt" for-parse-pattern)
         (only-in "unquote-binding-primitive.rkt"
                  #%parens)
         (only-in "quasiquote.rkt"
                  #%quotes)
         (only-in "dollar.rkt"
                  $)
         "parse.rkt")

(provide (for-space rhombus/bind
                    pattern))

;; the `pattern` form for a binding context is here;
;; the `pattern` form for `$` is in "unquote-binding-primitive.rkt"

(define-unquote-binding-syntax bounce-to-pattern
  (unquote-binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (parse-pattern #'tail)]))))

(define-binding-syntax pattern
  (binding-transformer
   (lambda (stx)
     (syntax-parse #`(group #%quotes
                            (quotes (group (op $)
                                           (parens ;; needs `#%parens` binding
                                            (group bounce-to-pattern . #,stx)))))
       [b::binding
        #:with b-form::binding-form #'b.parsed
        (values
         (binding-form
          #'pattern-infoer
          #`[#,(annotation-string-from-pattern (shrubbery-syntax->string stx))
             b-form.infoer-id b-form.data])
         #'())]))))

(define-syntax (pattern-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [annot-str infoer-id data])
     #:with impl::binding-impl #'(infoer-id up-static-infos data)
     #:with info::binding-info #'impl.info
     (binding-info #'annot-str
                   #'info.name-id
                   #'info.static-infos
                   #'info.bind-infos
                   #'info.oncer-id
                   #'info.matcher-id
                   #'info.evidence-ids
                   #'info.committer-id
                   #'info.binder-id
                   #'info.data)]))
