#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "dot-parse.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "realm.rkt"
         (submod "string.rkt" static-infos))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Srcloc))

(module+ for-builtin
  (provide srcloc-method-table))

(module+ for-static-info
  (provide (for-syntax srcloc-static-infos)))

(define-primitive-class Srcloc srcloc
  #:lift-declaration
  #:existing
  #:transparent
  #:fields
  ([(source)]
   [(line)]
   [(column)]
   [(position)]
   [(span)])
  #:properties
  ()
  #:methods
  (to_report_string
   ))

(define/method (Srcloc.to_report_string v)
  #:inline
  #:primitive (srcloc->string)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (srcloc->string v)))
