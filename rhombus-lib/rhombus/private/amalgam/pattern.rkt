#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (only-in shrubbery/print
                              shrubbery-syntax->string)
                     "annotation-string.rkt"
                     "tag.rkt")
         "binding.rkt"
         (submod "quasiquote.rkt" for-pattern)
         "unquote-binding.rkt"
         (submod "unquote-binding-primitive.rkt" for-parse-pattern)
         (only-in "unquote-binding-primitive.rkt"
                  #%parens)
         (only-in "dollar.rkt"
                  $))

(provide (for-space rhombus/bind
                    pattern))

;; the `pattern` form for a binding context is here;
;; the `pattern` form for `$` is in "unquote-binding-primitive.rkt"

(define-unquote-binding-syntax bounce-to-pattern
  (unquote-binding-transformer
   (lambda (stx ctx-kind)
     (syntax-parse stx
       [(_ . tail)
        (parse-pattern #'tail ctx-kind)]))))

(define-binding-syntax pattern
  (binding-transformer
   (lambda (stx)
     (do-quotes-binding #`(group (quotes
                                  (group (op $)
                                         (parens ; needs `#%parens` binding
                                          (group bounce-to-pattern . #,stx)))))
                        (lambda (e)
                          (syntax-parse stx
                            [(_ . tail)
                             (annotation-string-from-pattern
                              (shrubbery-syntax->string
                               #`(#,group-tag pattern . tail)))]))))))
