#lang racket/base
(require (for-syntax racket/base)
         racket/symbol
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         "call-result-key.rkt"
         "compare-key.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Keyword))

(module+ for-static-info
  (provide (for-syntax get-keyword-static-infos)))

(define-static-info-getter get-keyword-static-infos
  (#%compare ((< keyword<?)
              (<= keyword<=?)
              (= keyword=?)
              (!= keyword!=?)
              (>= keyword>=?)
              (> keyword>?))))

(define-annotation-syntax Keyword
  (identifier-annotation keyword? #,(get-keyword-static-infos)))

(define-name-root Keyword
  #:fields
  (from_string
   from_symbol))

(define/arity (from_string s)
  #:static-infos ((#%call-result #,(get-keyword-static-infos)))
  (string->keyword s))

(define/arity (from_symbol s)
  #:static-infos ((#%call-result #,(get-keyword-static-infos)))
  (string->keyword (symbol->immutable-string s)))

(define (check-keywords who a b)
  (unless (and (keyword? a) (keyword? b))
    (raise-annotation-failure who (if (keyword? a) b a) "Keyword")))

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
