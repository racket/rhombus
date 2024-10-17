#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/name-parse
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "dotted-sequence.rkt")
         syntax/parse/pre
         shrubbery/print
         "pack.rkt"
         "parse.rkt"
         "syntax-class-primitive.rkt"
         (only-in "expression.rkt"
                  in-expression-space)
         "pattern-variable.rkt"
         "unquote-binding.rkt"
         "unquote-binding-identifier.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "space.rkt"
         "parens.rkt"
         "dotted-sequence.rkt"
         (for-template (submod "syntax-meta.rkt" for-unquote)
                       "space.rkt")
         "annotation-failure.rkt")

;; see also "unquote-binding-primitive.rkt"; this one has only forms
;; that need meta-time bindings in expansion, so we don't want a
;; meta-time inclusion in `rhombus/meta` (which would then need a
;; meta-meta rhombus)

(provide (for-space rhombus/unquote_bind
                    bound_as))

(define-unquote-binding-syntax bound_as
  (unquote-binding-transformer
   (lambda (stxes)
     (cond
       [(eq? (current-unquote-binding-kind) 'term)
        (syntax-parse stxes
          #:datum-literals (group)
          [(form-id space ... (_::block (group (_::quotes (group bound::dotted-operator-or-identifier-sequence)))))
           (values #`((~seq (~var _ (:free=-in-space
                                     (quote-syntax (group . bound))
                                     (rhombus-expression (group space ...))
                                     'form-id)))
                      ()
                      ()
                      ())
                   #'())])]
       [else (values #'#f #'())]))))

(define-splicing-syntax-class (:free=-in-space bound-id space-path who)
  #:datum-literals (group)
  #:description (format "name bound as ~a" (shrubbery-syntax->string bound-id #:use-raw? #t))
  (pattern t::dotted-operator-or-identifier-sequence
           #:when (syntax_meta.equal_binding bound-id #'(group . t)
                                             (cond
                                               [(space-name? space-path) space-path]
                                               [else
                                                (raise-annotation-failure who space-path "SpaceMeta")]))))
