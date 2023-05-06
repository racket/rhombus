#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "class-parse.rkt")
         (submod "dot.rkt" for-dot-provider)
         "entry-point.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "function-arity.rkt"
         "static-info.rkt")

(provide (for-syntax build-class-static-infos))

(define-for-syntax (build-class-static-infos exposed-internal-id
                                             super
                                             given-constructor-rhs
                                             constructor-keywords constructor-defaults
                                             constructor-private-keywords constructor-private-defaults
                                             names)
  (with-syntax ([(name constructor-name name-instance
                       internal-name-instance make-internal-name
                       indirect-static-infos
                       [name-field ...]
                       [field-static-infos ...])
                 names])
    (append
     (list
      #`(define-static-info-syntax constructor-name
          (#%call-result ((#%dot-provider name-instance)
                          . indirect-static-infos))
          (#%function-arity #,(if given-constructor-rhs
                                  (syntax-parse given-constructor-rhs
                                    [(_ e-arity::entry-point-arity)
                                     (syntax->datum #'e-arity.parsed)])
                                  (summarize-arity constructor-keywords
                                                   constructor-defaults
                                                   #f #f)))))
     (if exposed-internal-id
         (list
          #`(define-static-info-syntax make-internal-name
              #,(let ([info #'(#%call-result ((#%dot-provider internal-name-instance)))])
                  (if super
                      ;; internal constructor is curried
                      #`(#%call-result (#,info))
                      info))))
         '())
     (list
      #'(begin
          (define-static-info-syntax/maybe* name-field (#%call-result field-static-infos))
          ...)))))

(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(_ id (_)) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))
