#lang racket/base
(require (for-syntax racket/base)
         racket/symbol
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "compare-key.rkt"
         (submod "annotation.rkt" for-class)
         "realm.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Keyword))

(module+ for-static-info
  (provide (for-syntax keyword-static-infos)))

(define-for-syntax keyword-static-infos
  #'((#%compare ((< keyword<?)
                 (<= keyword<=?)
                 (= keyword=?)
                 (!= keyword!=?)
                 (>= keyword>=?)
                 (> keyword>?)))))

(define-annotation-syntax Keyword
  (identifier-annotation #'keyword? keyword-static-infos))

(define-name-root Keyword
  #:fields
  (from_string
   from_symbol))

(define/arity (from_string s)
  #:static-infos ((#%call-result #,keyword-static-infos))
  (string->keyword s))

(define/arity (from_symbol s)
  #:static-infos ((#%call-result #,keyword-static-infos))
  (string->keyword (symbol->immutable-string s)))

(define (check-keywords who a b)
  (unless (and (keyword? a) (keyword? b))
    (raise-argument-error* who rhombus-realm "Keyword" (if (keyword? a) b a))))

(define (keyword<=? a b)
  (check-keywords '<= a b)
  (or (eq? a b) (keyword<? a b)))

(define (keyword=? a b)
  (check-keywords '= a b)
  (eq? a b))

(define (keyword!=? a b)
  (check-keywords '!= a b)
  (not (eq? a b)))

(define (keyword>=? a b)
  (check-keywords '>= a b)
  (or (eq? a b) (keyword<? b a)))

(define (keyword>? a b)
  (check-keywords '> a b)
  (keyword<? b a))
