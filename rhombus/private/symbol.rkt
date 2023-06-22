#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class))

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Symbol))

(define-annotation-syntax Symbol (identifier-annotation #'symbol? #'()))

(define-name-root Symbol
  #:fields
  (from_string
   uninterned_from_string
   unreadable_from_string
   gen))

(define/arity (from_string s)
  (string->symbol s))

(define/arity (uninterned_from_string s)
  (string->uninterned-symbol s))

(define/arity (unreadable_from_string s)
  (string->unreadable-symbol s))

(define/arity gen
  (case-lambda
    [(s) (gensym s)]
    [() (gensym)]))
