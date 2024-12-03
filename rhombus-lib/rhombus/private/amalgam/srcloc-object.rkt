#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "dot-parse.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "maybe-key.rkt"
         "define-arity.rkt"
         "realm.rkt"
         (submod "string.rkt" static-infos)
         (submod "arithmetic.rkt" static-infos))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Srcloc))

(module+ for-builtin
  (provide srcloc-method-table))

(module+ for-static-info
  (provide (for-syntax get-srcloc-static-infos)))

(define-primitive-class Srcloc srcloc
  #:lift-declaration
  #:existing
  #:transparent
  #:fields
  ([(source)]
   [(line) ((#%maybe #,(get-int-static-infos)))]
   [(column) ((#%maybe #,(get-int-static-infos)))]
   [(position) ((#%maybe #,(get-int-static-infos)))]
   [(span) ((#%maybe #,(get-int-static-infos)))])
  #:properties
  ()
  #:methods
  (to_report_string))

(define/method (Srcloc.to_report_string v)
  #:primitive (srcloc->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (srcloc->string v)))
