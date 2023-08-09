#lang racket/base
(require syntax/parse/pre
         (for-syntax racket/base
                     syntax/parse/pre)
         "op-literal.rkt"
         "pattern-variable.rkt"
         "pack.rkt"
         (only-in "static-info.rkt"
                  in-static-info-space
                  make-static-infos)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-syntax
          is-simple-pattern?
          maybe-bind-tail
          maybe-return-tail
          maybe-bind-all))

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

(define-for-syntax (maybe-bind-all all-id self-id make-all-id tail-pattern tail)
  (cond
    [(syntax-e all-id)
     (list
      #`(define-syntax #,(in-static-info-space all-id) (make-static-infos syntax-static-infos))
      (syntax-parse tail-pattern
        [() #`(define #,all-id #,self-id)]
        [(_ name _)
         #`(define #,all-id (#,make-all-id (cons #,self-id tail)))]))]
    [else '()]))
