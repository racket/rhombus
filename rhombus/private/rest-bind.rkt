#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt")

(provide (for-spaces (rhombus/bind)
                     rest-bind))

(define-binding-syntax rest-bind
  (binding-prefix-operator
   #'rest-bind
   '()
   'macro
   (lambda (tail)
     (syntax-parse tail
       [(_ static-infos rest-arg::binding)
        #:with rest::binding-form #'rest-arg.parsed
        (values
         (binding-form
          #'rest-bind-infoer
          #'[static-infos rest.infoer-id rest.data])
         #'())]))))

(define-syntax (rest-bind-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [static-infos rest-infoer-id rest-data])
     #:with all-static-infos (static-infos-union #'static-infos #'up-static-infos)
     #:with rest-impl::binding-impl #'(rest-infoer-id all-static-infos rest-data)
     #'rest-impl.info]))
