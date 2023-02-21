#lang racket/base
(require (for-syntax racket/base)
         racket/symbol
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class))

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Keyword))

(define-annotation-syntax Keyword (identifier-annotation #'keyword? #'()))

(define-name-root Keyword
  #:fields
  (from_string
   from_symbol))

(define/arity (from_string s)
  (string->keyword s))

(define/arity (from_symbol s)
  (string->keyword (symbol->immutable-string s)))
