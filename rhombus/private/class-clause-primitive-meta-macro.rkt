#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "macro-macro.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "macro-rhs.rkt"
                     (submod "dot.rkt" for-dot-provider)
                     "srcloc.rkt"
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
                     dot))

;; see also "class-clause-primitive-macro.rkt"; this one has only
;; forms that need meta-time bindings, so we don't want a mate-time
;; including of the works in `rhombus/meta` (which would then need a
;; meta-meta rhombus)

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
                                            (lambda (p ct)
                                              ct)
                                            (lambda (ps ct)
                                              ct))]))

  (define-syntax (dot-transformer-compiletime stx)
    (syntax-parse stx
      [(_ pre-parseds self-ids extra-argument-ids)
       (parse-transformer-definition-rhs (syntax->list #'pre-parseds)
                                         (syntax->list #'self-ids)
                                         (syntax->list #'extra-argument-ids)
                                         #'values
                                         #`(syntax-static-infos #'() syntax-static-infos)
                                         #:else #'#f)])))
