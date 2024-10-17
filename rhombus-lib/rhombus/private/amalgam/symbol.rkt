#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class)
         "compare-key.rkt"
         "call-result-key.rkt"
         "static-info.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Symbol))

(module+ for-static-info
  (provide (for-syntax get-symbol-static-infos)))

(define-static-info-getter get-symbol-static-infos
  (#%compare ((< symbol<?)
              (<= symbol<=?)
              (= symbol=?)
              (!= symbol!=?)
              (>= symbol>=?)
              (> symbol>?))))

(define-annotation-syntax Symbol
  (identifier-annotation symbol? #,(get-symbol-static-infos)))

(define-name-root Symbol
  #:fields
  (from_string
   uninterned_from_string
   unreadable_from_string
   gen))

(define/arity (from_string s)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (string->symbol s))

(define/arity (uninterned_from_string s)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (string->uninterned-symbol s))

(define/arity (unreadable_from_string s)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (string->unreadable-symbol s))

(define/arity gen
  (case-lambda
    [(s) (gensym s)]
    [() (gensym)]))

(define (check-symbols who a b)
  (unless (and (symbol? a) (symbol? b))
    (raise-annotation-failure who (if (symbol? a) b a) "Symbol")))

(define (symbol<=? a b)
  (check-symbols '<= a b)
  (or (eq? a b) (symbol<? a b)))

(define (symbol=? a b)
  (check-symbols '= a b)
  (eq? a b))

(define (symbol!=? a b)
  (check-symbols '!= a b)
  (not (eq? a b)))

(define (symbol>=? a b)
  (check-symbols '>= a b)
  (or (eq? a b) (symbol<? b a)))

(define (symbol>? a b)
  (check-symbols '> a b)
  (symbol<? b a))
