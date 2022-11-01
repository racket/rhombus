#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "class-parse.rkt")
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "static-info.rkt")

(provide (for-syntax build-class-static-infos))

(define-for-syntax (build-class-static-infos exposed-internal-id
                                             names)
  (with-syntax ([(name constructor-name name-instance
                       [name-field ...]
                       [field-static-infos ...])
                 names])
    (append
     (list
      #'(define-static-info-syntax constructor-name (#%call-result ((#%dot-provider name-instance)))))
     (if exposed-internal-id
         (list
          #`(define-static-info-syntax #,exposed-internal-id (#%call-result ((#%dot-provider name-instance)))))
         '())
     (list
      #'(begin
          (define-static-info-syntax/maybe* name-field (#%call-result field-static-infos))
          ...)))))

(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(_ id (_)) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))
