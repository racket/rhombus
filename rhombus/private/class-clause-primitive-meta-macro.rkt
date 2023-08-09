#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "macro-macro.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "macro-rhs.rkt"
                     (submod "dot.rkt" for-dot-provider)
                     "srcloc.rkt"
                     "parse.rkt"
                     (for-syntax racket/base
                                 syntax/parse/pre
                                 "with-syntax.rkt"
                                 "srcloc.rkt"))
         "provide.rkt"
         "class-clause.rkt"
         "class-clause-tag.rkt"
         "interface-clause.rkt"
         "op-literal.rkt"
         "parens.rkt")

(provide (for-spaces (rhombus/class_clause
                      rhombus/interface_clause)
                     dot
                     static_info))

;; see also "class-clause-primitive-macro.rkt"; this one has only
;; forms that need meta-time bindings, so we don't want a meta-time
;; inclusion in `rhombus/meta` (which would then need a meta-meta
;; rhombus)

(define-for-syntax (make-macro-clause-transformer
                    #:clause-transformer [clause-transformer class-clause-transformer])
  (clause-transformer
   (lambda (stx data)
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
                               #,(no-srcloc
                                  #`(class-dot-transformer
                                     (form-name (q-tag (g-tag dot
                                                              d1 left
                                                              d1 dot-op
                                                              name))
                                                template-block))))))]))))

(define-class-clause-syntax dot
  (make-macro-clause-transformer))

(define-interface-clause-syntax dot
  (make-macro-clause-transformer #:clause-transformer interface-clause-transformer))

(begin-for-syntax
  (define-syntax (class-dot-transformer stx)
    (syntax-parse stx
      #:literals ()
      #:datum-literals (group named-macro)
      [(_ pat)
       (parse-identifier-syntax-transformer #'pat
                                            #'dot-transformer-compiletime
                                            '(#:head_stx #:is_static #:tail)
                                            '(value value pattern)
                                            (lambda (p ct)
                                              ct)
                                            (lambda (ps ct)
                                              ct))]))

  (define-syntax (dot-transformer-compiletime stx)
    (syntax-parse stx
      [(_ pre-parseds self-ids all-ids extra-argument-binds)
       (parse-transformer-definition-rhs (syntax->list #'pre-parseds)
                                         (syntax->list #'self-ids)
                                         (syntax->list #'all-ids)
                                         (syntax->list #'extra-argument-binds)
                                         #'values
                                         #`(syntax-static-infos #'() syntax-static-infos)
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
