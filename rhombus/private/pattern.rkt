#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (only-in shrubbery/print
                              shrubbery-syntax->string)
                     "annotation-string.rkt"
                     "srcloc.rkt")
         "binding.rkt"
         (only-in "unquote-binding-primitive.rkt"
                  pattern
                  #%parens)
         (only-in "quasiquote.rkt"
                  #%quotes)
         (only-in "dollar.rkt"
                  $)
         "parse.rkt"
         "parens.rkt")

(provide (for-space rhombus/bind
                    pattern))

;; the `pattern` form for a binding context is here;
;; the `pattern` form for `$` is in "unquote-binding-primitive.rkt"

(define-binding-syntax pattern
  (binding-transformer
   (lambda (stx)
     (define (expand s)
       (syntax-parse #`(group #%quotes
                              (quotes (group (op $)
                                             (parens ;; needs `#%parens` binding
                                              (group . #,s)))))
         [b::binding
          #:with b-form::binding-form #'b.parsed
          (binding-form
           #'pattern-infoer
           #`[#,(annotation-string-from-pattern (shrubbery-syntax->string stx))
              b-form.infoer-id b-form.data])]))
     (with-syntax ([pattern (syntax-parse stx
                              [(head . _)
                               (datum->syntax #'here 'pattern #'head #'head)])])
       (syntax-parse stx
         [(_ . tail)
          (values (expand (relocate+reraw
                           (respan stx)
                           #'(pattern . tail)))
                  #'())])))))

(define-syntax (pattern-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [annot-str infoer-id data])
     #:with impl::binding-impl #'(infoer-id up-static-infos data)
     #:with info::binding-info #'impl.info
     (binding-info #'annot-str
                   #'info.name-id
                   #'info.static-infos
                   #'info.bind-infos
                   #'info.matcher-id
                   #'info.committer-id
                   #'info.binder-id
                   #'info.data)]))
