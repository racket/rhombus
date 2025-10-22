#lang racket/base
(require (for-syntax racket/base)
         "define-arity.rkt"
         "provide.rkt"
         "call-result-key.rkt"
         (submod "arithmetic.rkt" static-infos))

(provide (for-spaces (#f
                      rhombus/statinfo)
                     Function.count))

(define/arity (Function.count . args)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (length args))
