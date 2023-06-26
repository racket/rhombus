#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "dot-parse.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
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

(define/arity #:name Srcloc.to_report_string (srcloc->report-string s)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (srcloc->string s)))

(define srcloc->report-string/method (method1 srcloc->report-string))

(define (srcloc->string/method s)
  (lambda ()
    (srcloc->string s)))

(define-primitive-class Srcloc srcloc
  #:constructor-static-info ()
  #:existing
  #:transparent
  #:fields
  ([source ()]
   [line ()]
   [column ()]
   [position ()]
   [span ()])
  #:properties
  ()
  #:methods
  ([to_report_string 1 srcloc->report-string srcloc->report-string/method string-static-infos]))
