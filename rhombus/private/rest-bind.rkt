#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "binding.rkt"
         "parse.rkt")

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
    [(_ _ [static-infos rest-infoer-id rest-data])
     #:with rest-impl::binding-impl #'(rest-infoer-id static-infos rest-data)
     #'rest-impl.info]))
