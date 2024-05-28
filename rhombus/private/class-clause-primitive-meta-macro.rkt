#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (submod "syntax-object.rkt" for-quasiquote)
                     "macro-rhs.rkt"
                     "parse.rkt"
                     (only-in "static-info.rkt"
                              get-empty-static-infos)
                     (for-syntax racket/base
                                 syntax/parse/pre))
         "provide.rkt"
         "class-clause.rkt"
         "class-clause-tag.rkt"
         "interface-clause.rkt"
         "veneer-clause.rkt"
         "op-literal.rkt"
         "macro-macro.rkt"
         "parens.rkt")

(provide (for-spaces (rhombus/class_clause
                      rhombus/interface_clause
                      rhombus/veneer_clause)
                     dot
                     static_info))

;; see also "class-clause-primitive-macro.rkt"; this one has only
;; forms that need meta-time bindings, so we don't want a meta-time
;; inclusion in `rhombus/meta` (which would then need a meta-meta
;; rhombus)

(define-for-syntax (parse-dot stx data)
  (syntax-parse stx
    #:datum-literals (group op |.|)
    [(form-name (q-tag::quotes ((~and g-tag group)
                                d1::$-bind
                                left:identifier
                                (op |.|)
                                name:identifier))
                (~and (_::block . _)
                      template-block))
     (wrap-class-clause #`(#:dot
                           name
                           (block
                            #,(class-dot-transformer
                               #'(form-name (q-tag (g-tag dot
                                                          d1 left
                                                          d1 dot-op
                                                          name))
                                            template-block)))))]))

(define-class-clause-syntax dot
  (class-clause-transformer parse-dot))

(define-interface-clause-syntax dot
  (interface-clause-transformer parse-dot))

(define-veneer-clause-syntax dot
  (veneer-clause-transformer parse-dot))

(begin-for-syntax
  (define (class-dot-transformer pat)
    (parse-identifier-syntax-transformer pat
                                         #'dot-transformer-compiletime
                                         '(#:head_stx #:is_static #:tail)
                                         '(value value pattern)
                                         (lambda (p ct)
                                           ct)
                                         (lambda (ps ct)
                                           ct)))

  (define-syntax (dot-transformer-compiletime stx)
    (syntax-parse stx
      [(_ pre-parseds self-ids all-ids extra-argument-binds)
       (parse-transformer-definition-rhs (syntax->list #'pre-parseds)
                                         (syntax->list #'self-ids)
                                         (syntax->list #'all-ids)
                                         (syntax->list #'extra-argument-binds)
                                         #'values
                                         #'(get-syntax-static-infos get-empty-static-infos get-syntax-static-infos)
                                         '(value value pattern)
                                         #:else #'#f
                                         #:cut? #t)])))


(define-for-syntax (parse-static_info stx data)
  (syntax-parse stx
    [(_ (tag::block body ...))
     (wrap-class-clause #`(#:static-infos
                           (rhombus-body-at tag body ...)))]))

(define-class-clause-syntax static_info
  (class-clause-transformer parse-static_info))

(define-interface-clause-syntax static_info
  (interface-clause-transformer parse-static_info))

(define-veneer-clause-syntax static_info
  (veneer-clause-transformer parse-static_info))
