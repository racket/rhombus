#lang racket/base
(require syntax/parse/pre
         (for-syntax racket/base
                     syntax/parse/pre)
         "op-literal.rkt"
         "pattern-variable.rkt"
         "pack.rkt")

(provide (for-syntax
          is-simple-pattern?
          maybe-bind-tail
          maybe-return-tail))

(define-for-syntax (is-simple-pattern? tail-pattern)
  (syntax-parse tail-pattern
    [() #t]
    [(_::$-bind name:identifier _::...-bind)
     #t]
    [_ #f]))

(define-for-syntax (maybe-bind-tail tail-pattern tail)
  (syntax-parse tail-pattern
    [() null]
    [(_ name _)
     (list
      #`(define-syntaxes . #,(make-pattern-variable-bind #'name tail #'unpack-tail-list* 1 '())))]))

;; add tail or not for an `enforest`-based simple pattern
(define-for-syntax (maybe-return-tail expr tail-pattern tail)
  (syntax-parse tail-pattern
    [() #`(values #,expr #,tail)]
    [(_ name _) expr]))
